{-|
Module      : TreeGen
Description : Generate and populate binary trees
Copyright   : Scott Pakin, 2014
License     : GPL-3
Maintainer  : scott-fexpr@pakin.org
Stability   : experimental
Portability : portable

Produce a list of all binary trees with unit in each internal node.
These trees are sorted by nondecreasing number of internal nodes.
Provide a function to replace units with given values.

-}

module TreeGen ( Tree(..)
               , evaluateTree
               , treeStructures
               , tallyTreeNodes
               , replaceOperators ) where

-- | Represent a tree with values at the leaves and unary and binary
-- operators at internal nodes.
data Tree u b = Val                        -- ^ A value at a leaf of the tree
          | UnOp u (Tree u b)              -- ^ A unary operator applied to a subtree
          | BinOp b (Tree u b) (Tree u b)  -- ^ A binary operator applied to two subtrees
          deriving Show

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

-- | Convert a Schr&#x00F6;der path to a binary tree.
pathToTree :: [PathStep] -> Tree () ()
pathToTree ps = fst $ pathToTree' ps
  where pathToTree' :: [PathStep] -> (Tree () (), [PathStep])
        pathToTree' [] = (Val, [])
        pathToTree' (NE:ps) = (UnOp () childTree, restOfPath)
          where (childTree, restOfPath) = pathToTree' ps
        pathToTree' (E:ps) = (BinOp () leftTree rightTree, restOfPath)
          where (leftTree, leftRemainder) = pathToTree' ps
                (rightTree, restOfPath) = pathToTree' leftRemainder
        pathToTree' (N:ps) = (Val, ps)

-- | Return a list of all tree structures in nondecreasing order of
-- operator count (&#x2248; complexity).
treeStructures :: [Tree () ()]
treeStructures =
  let allSchroederPaths = concat [schroederPaths e e | e <- [0..]]
  in  [pathToTree p | p <- allSchroederPaths]

-- | Substitute all unary and binary operators in a tree with the
-- values provided.  That is, the structure of the tree remains the
-- same; only the operators change.
replaceOperators :: (Tree v w) -> [u] -> [b] -> (Tree u b)
replaceOperators tree us bs = fst3 $ replaceOperators' tree us bs
  where fst3 (x, _, _) = x
        replaceOperators' Val us bs = (Val, us, bs)
        replaceOperators' (UnOp _ t) (u:us) bs = (UnOp u t', us', bs')
          where (t', us', bs') = replaceOperators' t us bs
        replaceOperators' (BinOp _ left right) us (b:bs) = (BinOp b left' right', rightUs, rightBs)
          where (left', leftUs, leftBs) = replaceOperators' left us bs
                (right', rightUs, rightBs) = replaceOperators' right leftUs leftBs

-- | Given a tree populated with unary and binary functions and a list
-- of values, reduce the tree to a single value.
evaluateTree :: (Tree (a -> a) (a -> a -> a)) -> [a] -> a
evaluateTree tree values = fst $ evaluateTree' tree values
  where evaluateTree' Val (v:vs) = (v, vs)
        evaluateTree' (UnOp u t) vs = (u val, vs')
          where (val, vs') = evaluateTree' t vs
        evaluateTree' (BinOp b t1 t2) vs = (b left right, rightVals)
          where (left, leftVals) = evaluateTree' t1 vs
                (right, rightVals) = evaluateTree' t2 leftVals

-- | Tally the number of unary, binary, and value nodes in a tree.
tallyTreeNodes :: Tree u b -> (Int, Int, Int)
tallyTreeNodes Val = (0, 0, 1)
tallyTreeNodes (UnOp _ t) = (u + 1, b, v)
  where (u, b, v) = tallyTreeNodes t
tallyTreeNodes (BinOp _ t1 t2) = (u1 + u2, b1 + b2 + 1, v1 + v2)
  where (u1, b1, v1) = tallyTreeNodes t1
        (u2, b2, v2) = tallyTreeNodes t2
