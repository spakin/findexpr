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
    otherwise -> (if w < h then goN else []) ++ goE ++ goNE
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

-- | Return every other element of a list, starting with the first, as
-- in the following examples:
--
-- >>> everyOther "steadfastness"
-- "sedates"
-- >>> (everyOther . tail) "fruitfulness"
-- "rifles"
everyOther :: [a] -> [a]
everyOther (e:o:xs) = e : everyOther xs
everyOther [e] = [e]
everyOther _ = []

-- | Perform a single riffle shuffle of a list.  That is, return the
-- concatenation of all elements at even-numbered positions and all
-- elements at odd-numbered positions.  Consider the following
-- examples:
--
-- >>> riffle [1..20]
-- [1,3,5,7,9,11,13,15,17,19,2,4,6,8,10,12,14,16,18,20]
-- >>> (riffle . riffle) [1..20]
-- [1,5,9,13,17,2,6,10,14,18,3,7,11,15,19,4,8,12,16,20]
riffle :: [a] -> [a]
riffle lst@(x:xs) = evens ++ odds
  where evens = everyOther lst
        odds = (everyOther . tail) lst
riffle [] = []

-- | Return a list of all lists of stack operations in nondecreasing
-- order of operator count (&#x2248; complexity).  For a given
-- operator count, lists are returned in arbitrary order (in fact,
-- based on three riffle shuffles on the output from
-- 'schroederPaths').
allStackOps :: [[StackOp () ()]]
allStackOps = concat [riffle3 . map pathToStackOps $ schroederPaths e e | e <- [0..]]
  where riffle3 = riffle . riffle . riffle

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
