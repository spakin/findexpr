{-|
Module      : FindExpr
Description : Find all expressions that map inputs to outputs
Copyright   : Scott Pakin, 2014
License     : GPL-3
Maintainer  : scott-fexpr@pakin.org
Stability   : experimental
Portability : portable

Use a brute-force approach to find all expressions that map a list of
inputs to an output value for all given lists of inputs and all given
output values.
-}

module FindExpr ( findAllExpressions ) where

import ParseInput
import Data.List
import Data.Maybe

-- | Represent a tree with values at the leaves and unary and binary
-- operators at internal nodes.
data Tree u b = Val                        -- ^ A value at a leaf of the tree
          | UnOp u (Tree u b)              -- ^ A unary operator applied to a subtree
          | BinOp b (Tree u b) (Tree u b)  -- ^ A binary operator applied to two subtrees
          deriving Show

-- | Given a number of operators (unary + binary), return all trees
-- with that many operators.  At this stage we consider only the
-- structure of each tree and therefore return each tree typed as
-- @Tree () ()@.
treesOfOpCount :: Integer -> [Tree () ()]
treesOfOpCount 0 = [Val]
treesOfOpCount n = unaryTrees ++ binaryTrees
  where unaryTrees = [UnOp () t | t <- treesOfOpCount (n - 1)]
        binaryTrees = [BinOp () t1 t2 |
                       n' <- [0 .. n - 1],
                       t1 <- treesOfOpCount n',
                       t2 <- treesOfOpCount (n - 1 - n')]

-- | Return a list of all tree structures in nondecreasing order of
-- operator count (&#x2248; complexity).
treeStructures :: [Tree () ()]
treeStructures = concatMap treesOfOpCount [0..]

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

-- | Given a finite list, return a infinite list of infinite lists
-- in which each element appears in each position of some list, with
-- duplication.  Consider the following example:
--
-- >>> take 10 $ map (take 5) $ allValuesAllPositions "ABC"
-- ["AAAAA","BAAAA","CAAAA","ABAAA","BBAAA","CBAAA","ACAAA","BCAAA","CCAAA","AABAA"]
allValuesAllPositions :: [a] -> [[a]]
allValuesAllPositions [] = [[]]
allValuesAllPositions values = (transpose . map cycle) allReplications
  where
    numElts = length values
    replicateElts [] = []
    replicateElts (x:xs) = (replicate numElts x) ++ replicateElts xs
    allReplications = iterate replicateElts values

-- | Given a finite list and a number of selections to make with
-- replacement, return a finite list of finite lists in which each
-- element appears in each position of some list.  Consider the
-- following example:
--
-- >>> allValuesNPositions "ABC" 4
-- ["AAAA","AAAB","AAAC","AABA","AABB","AABC","AACA","AACB","AACC","ABAA","ABAB","ABAC","ABBA","ABBB","ABBC","ABCA","ABCB","ABCC","ACAA","ACAB","ACAC","ACBA","ACBB","ACBC","ACCA","ACCB","ACCC","BAAA","BAAB","BAAC","BABA","BABB","BABC","BACA","BACB","BACC","BBAA","BBAB","BBAC","BBBA","BBBB","BBBC","BBCA","BBCB","BBCC","BCAA","BCAB","BCAC","BCBA","BCBB","BCBC","BCCA","BCCB","BCCC","CAAA","CAAB","CAAC","CABA","CABB","CABC","CACA","CACB","CACC","CBAA","CBAB","CBAC","CBBA","CBBB","CBBC","CBCA","CBCB","CBCC","CCAA","CCAB","CCAC","CCBA","CCBB","CCBC","CCCA","CCCB","CCCC"]
allValuesNPositions :: [a] -> Int -> [[a]]
allValuesNPositions values positions = take numRows $ map (reverse . take numCols) allPossibilities
  where
    numCols = positions
    numRows = (length values)^numCols
    allPossibilities = allValuesAllPositions values

-- | Given a finite list and a number of selections to make with
-- replacement, return a finite list of finite lists of length-/N/
-- permutations (with some duplication) in which each element appears
-- in at most one position.  (/N/ should be no greater than the length
-- of the list.)  Consider the following example:
--
-- >>> uniqueValuesNPositions "ABCD" 2
-- ["AB","BA","CB","BC","CA","AC","DC","CD","CB","DB","BD","BC","DA","AD","AB","DB","BD","BA","DA","AD","AC","DC","CD","CA"]
-- >>> nub it
-- ["AB","BA","CB","BC","CA","AC","DC","CD","DB","BD","DA","AD"]
uniqueValuesNPositions :: [a] -> Int -> [[a]]
uniqueValuesNPositions lst n = map (take n) $ permutations lst

-- | Return a list of inputs and corresponding list of column names in
-- a variety of length-/N/ permutations, either with repetitions
-- ('False') or without repetitions ('True').
permuteInputs :: (Eq a, Eq b) => Bool -> [[a]] -> [b] -> Int -> [([[a]], [b])]
permuteInputs unique inputs colNames numVals =
  if unique then
    let iPerms = transpose . map (flip uniqueValuesNPositions numVals) $ inputs
        cPerms = uniqueValuesNPositions colNames numVals
    in nub $ zip iPerms cPerms
  else
    let iPerms = transpose $ flip (\n -> map (flip allValuesNPositions n)) inputs numVals
        cPerms = allValuesNPositions colNames numVals
    in zip iPerms cPerms

-- | Report whether a tree reduces to a given value.
validateTree :: (Eq a) => (Tree (a -> a) (a -> a -> a)) -> ([a], a) -> Bool
validateTree tree (inputs, output) = evaluateTree tree inputs == output

-- | Find all expressions that hold for all pairs of inputs and
-- outputs.  The following example finds three functions /f/ such that
-- /f/(1,2,3)=13 and /f/(10,20,30)=1210:
--
-- >>> let stringPrefix p s = p ++ "(" ++ s ++ ")"
-- >>> let stringOp opStr a b = "(" ++ a ++ ") " ++ opStr ++ " (" ++ b ++ ")"
-- >>> let unaryTable = [UnaryOperator ("-", liftM negate, stringPrefix "-")]
-- >>> let binaryTable = [BinaryOperator ("+", liftM2 (+), stringOp "+"), BinaryOperator ("*", liftM2 (*), stringOp "*")]
-- >>> take 3 $ findAllExpressions unaryTable binaryTable [[1,2,3], [10,20,30]] ["A","B","C"] [13,1210] Repeated
-- ["(A) + ((B) * ((C) + (C)))","(A) + ((C) * ((A) + (C)))","(A) + ((C) * ((B) + (B)))"]
--
-- 'findAllExpressions' makes no attempt to simplify the expressions
-- or even eliminate unnecessary parentheses.  One may wish to
-- postprocess the output using a tool such as
-- <http://www.wolframalpha.com/ Wolfram|Alpha> to reduce the
-- preceding three expressions to /A/+2/BC/, /A/+/C/(/A/+/C/), and
-- /A/+2/BC/ again.
findAllExpressions :: [UnaryOperator]   -- ^ List of unary operators to try
                   -> [BinaryOperator]  -- ^ List of binary operators to try
                   -> [[Int]]           -- ^ Rows of columns of input values
                   -> [String]          -- ^ Name for each column of input
                   -> [Int]             -- ^ Rows of output values
                   -> Permutation       -- ^ Type of permutation to use
                   -> [String]          -- ^ An infinite list of expressions that map inputs to outputs
findAllExpressions unaryOps binaryOps inputs colNames outputs inputPermType =
  findAllExpressions' genUnaryPerms genBinaryPerms genInputPerms maybeOutputs
  where
    maybeInputs = [map Just i | i <- inputs] -- Inputs lifted to 'Maybe's
    maybeOutputs = map Just outputs          -- Outputs lifted to 'Maybe's
    numInputs = length colNames              -- Number of input columns
    genUnaryPerms = allValuesNPositions unaryOps    -- Generate unary-operator permutations
    genBinaryPerms = allValuesNPositions binaryOps  -- Generate binary-operator permutations
    genInputPerms =                                 -- Generate input and column-name permutations
      case inputPermType of
        Repeated -> permuteInputs False maybeInputs colNames
        Unique   -> permuteInputs True maybeInputs colNames
        UniqueAllPresent -> (\_ -> zip (permutations maybeInputs) (permutations colNames))
        RepeatedAllPresent -> (\n -> let perms = permuteInputs False maybeInputs colNames n
                                         haveAll (_, c) = length (nub c) == numInputs
                                     in filter haveAll perms)

-- | Do most of the work for 'findAllExpressions'.
findAllExpressions' :: (Int -> [[UnaryOperator]])            -- ^ Generate permutations of /n/ unary operators
                    -> (Int -> [[BinaryOperator]])           -- ^ Generate permutations of /n/ binary operators
                    -> (Int -> [([[Maybe Int]], [String])])  -- ^ Generate permutations of /n/ input-list and column-name tuples
                    -> [Maybe Int]                           -- ^ Output values to compare to
                    -> [String]                              -- ^ An infinite list of expressions that map inputs to outputs
findAllExpressions' genUnaryPerms genBinaryPerms genInputPerms maybeOutputs =
  [evaluateTree treeString isPerms |
   -- Iterate over all tree structures.
   treeStruct <- treeStructures,
   let (numUnOps, numBinOps, numVals) = tallyTreeNodes treeStruct,
   -- Iterate over all sets of unary operators.
   allUPerms <- genUnaryPerms numUnOps,
   let uiPerms = extractUIntFuncs allUPerms,
   let usPerms = extractUStringFuncs allUPerms,
   -- Iterate over all sets of binary operators.
   allBPerms <- genBinaryPerms numBinOps,
   let biPerms = extractBIntFuncs allBPerms,
   let bsPerms = extractBStringFuncs allBPerms,
   -- Plug the current operators into the current tree.
   let treeInt = replaceOperators treeStruct uiPerms biPerms,
   let treeString = replaceOperators treeStruct usPerms bsPerms,
   -- Iterate over all permutations of the inputs.
   (iiPerms, isPerms) <- genInputPerms numVals,
   length isPerms == numVals,
   -- Succeed if every input row produces the
   -- corresponding output value.
   all (validateTree treeInt) (zip iiPerms maybeOutputs)]
  where extractUIntFuncs = map (\(UnaryOperator (_, i, _)) -> i)
        extractUStringFuncs = map (\(UnaryOperator (_, _, s)) -> s)
        extractBIntFuncs = map (\(BinaryOperator (_, i, _)) -> i)
        extractBStringFuncs = map (\(BinaryOperator (_, _, s)) -> s)
