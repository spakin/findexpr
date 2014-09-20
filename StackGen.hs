{-|
Module      : StackGen
Description : Generate and populate expression stacks
Copyright   : Scott Pakin, 2014
License     : GPL-3
Maintainer  : scott-fexpr@pakin.org
Stability   : experimental
Portability : portable

Produce a list of all expression stacks with unit for the operator.
These stacks are sorted by nondecreasing number of operations.
Provide a function to replace units with given functions and apply
those to a list of values.

-}

module StackGen ( StackOp(..)
                , evaluateStack
                , allStackOps
                , tallyStackOpTypes
                , replaceOperators ) where

import Data.List

-- | A 'PathStep' is a step toward the east, north, or northeast.
data PathStep = E | N | NE
              deriving (Eq, Show)

-- | Return all paths from the southwest corner of an /M/&#x00D7;/N/
-- grid to the northeast corner that don't stray above the
-- antidiagonal.
schroederPaths :: Int -> Int -> [[PathStep]]
schroederPaths w h =
  case (w, h) of
    (0, 0)    -> [[]]
    (0, _)    -> goN
    otherwise -> goNE ++ goE ++ (if w < h then goN else [])
  where goN  = [N:ps  | ps <- schroederPaths w (h - 1)]
        goE  = [E:ps  | ps <- schroederPaths (w - 1) h]
        goNE = [NE:ps | ps <- schroederPaths (w - 1) (h - 1)]

-- | A 'StackOp' represents a single operation to perform on a stack.
data StackOp u b = Push1        -- ^ Push a value on the stack
                 | Pop1Push1 u  -- ^ Pop a value, apply a unary operator, and push the result
                 | Pop2Push1 b  -- ^ Pop two values, apply a binary operator, and push the result
                 deriving Show

-- | Convert a Schr&#x00F6;der path to a list of 'StackOp's.
pathToStackOps :: [PathStep] -> [StackOp () ()]
pathToStackOps steps = Push1 : (map stepToOp steps)
  where stepToOp E  = Push1
        stepToOp NE = Pop1Push1 ()
        stepToOp N  = Pop2Push1 ()

-- | Return a list of all lists of stack operations in nondecreasing
-- order of operator count (&#x2248; complexity).
allStackOps :: [[StackOp () ()]]
allStackOps = concat [map pathToStackOps $ schroederPaths e e | e <- [0..]]

-- | Substitute all unary and binary operators in a list of stack
-- operations with the values provided.
replaceOperators :: [StackOp p q] -> [u] -> [v] -> [StackOp u v]
replaceOperators ss us bs = reverse $ replaceOperators' ss us bs []
  where replaceOperators' (Push1:os) us bs st =
          replaceOperators' os us bs (Push1:st)
        replaceOperators' (Pop1Push1 _ : os) (u:us) bs st =
          replaceOperators' os us bs (Pop1Push1 u : st)
        replaceOperators' (Pop2Push1 _ : os) us (b:bs) st =
          replaceOperators' os us bs (Pop2Push1 b : st)
        replaceOperators' [] _ _ st = st

-- | Given a list of stack operations and a list of values, perform
-- the stack operations on the values, producing a single, final
-- value.
evaluateStack :: [StackOp (a -> a) (a -> a -> a)] -> [a] -> a
evaluateStack (Push1 : ops) (v:vs) = evaluateStack' ops vs v []
  where evaluateStack' (Push1 : os) (v:vs) s ss =
          evaluateStack' os vs v (s:ss)
        evaluateStack' (Pop1Push1 u : os) vs s ss =
          let us = u s in us `seq` evaluateStack' os vs us ss
        evaluateStack' (Pop2Push1 b : os) vs s2 (s1:ss) =
          let bss = b s1 s2 in bss `seq` evaluateStack' os vs bss ss
        evaluateStack' [] _ s _ = s

-- | Tally the number of unary, binary, and value nodes in a list of
-- stack operations.
tallyStackOpTypes :: [StackOp p q] -> (Int, Int, Int)
tallyStackOpTypes = foldl' tallyOne (0, 0, 0)
  where tallyOne (u, b, v) Push1         = (u,     b,     v + 1)
        tallyOne (u, b, v) (Pop1Push1 _) = (u + 1, b,     v)
        tallyOne (u, b, v) (Pop2Push1 _) = (u,     b + 1, v)
