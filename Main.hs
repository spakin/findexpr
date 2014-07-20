module Main where

import Data.List
import System.Environment
import System.FilePath
import Text.ParserCombinators.Parsec
import ParseInput
import FindExpr

-- | Return the base name of the executable for use in error messages.
getErrorMessageName :: IO String
getErrorMessageName =
  do
    fullName <- getProgName
    return $ takeBaseName fullName

-- | Convert a 'ParsedInput' to a tuple of arguments to be passed to
-- 'FindAllExpressions' plus a list of output column names.
produceFindExprArgs :: ParsedInput
                    -> ([UnaryOperator], [BinaryOperator],
                        [[Integer]], [[Integer]],
                        [String], [String],
                        Permutation)
produceFindExprArgs parseInfo =
  (unOps, binOps, inputs, outputs, inColNames, outColNames, Repeated)
  where unOps = unaryFuncs parseInfo
        binOps = binaryFuncs parseInfo
        (inputs, outputs) = unzip $ dataTable parseInfo
        (inColNames, outColNames) = colNames parseInfo

-- | Convert an 'Integer' to an 'Int', returning the original number
-- if it's not representable by an 'Int'.
integerToInt :: Integer -> Either Integer Int
integerToInt integerVal =
  let intVal = (fromIntegral integerVal) :: Int
      integerVal' = (fromIntegral intVal) :: Integer
  in if integerVal == integerVal'
     then Right intVal
     else Left integerVal

-- | Apply 'integerToInt' to a list of 'Integer' values, returning one
-- of the unconvertable numbers on failure.
integerListToIntList :: [Integer] -> Either Integer [Int]
integerListToIntList integerList = foldr processInteger (Right []) integerList
  where processInteger :: Integer -> Either Integer [Int] -> Either Integer [Int]
        processInteger _      err@(Left _) = err
        processInteger newVal (Right xs)   =
          case integerToInt newVal of (Right x)  -> Right (x:xs)
                                      (Left err) -> Left err

-- | Apply 'integerToInt' to a list of lists of 'Integer' values,
-- returning one of the unconvertable numbers on failure.
integerListListToIntListList :: [[Integer]] -> Either Integer [[Int]]
integerListListToIntListList integerLL = foldr processIntegerList (Right []) integerLL
  where processIntegerList :: [Integer] -> Either Integer [[Int]] -> Either Integer [[Int]]
        processIntegerList _      err@(Left _) = err
        processIntegerList newList (Right xs)   =
          case integerListToIntList newList of (Right x)  -> Right (x:xs)
                                               (Left err) -> Left err

-- | Invoke 'integerListListToIntListList'.  Return the result on
-- success, and abort the program on failure.
convertIntegersOrFail :: [[Integer]] -> IO [[Int]]
convertIntegersOrFail lst =
  let badIntMsg i = show i ++ " is out of range for an integer"
  in case integerListListToIntListList lst
     of Left badInt    -> fail $ badIntMsg badInt
        Right goodInts -> return goodInts

-- | Given multiple output expressions and a list of output variable
-- names, produce a single string to display.
formatOutputs :: [[String]] -> [String] -> String
formatOutputs oss ns =
  intercalate "\n" [formatOutputs' os n | (os, n) <- zip oss ns]
  where formatOutputs' [] [] = ""
        formatOutputs' os n = concatMap (\o -> n ++ " = " ++ o ++ "\n") os

main =
  do
    -- Parse the input file.
    progName <- getErrorMessageName
    problemText <- getContents
    let possibleParse = parse entireInput progName problemText
    problemInfo <- case possibleParse of Left errMsg  -> fail $ show errMsg
                                         Right record -> return record

    -- Convert all 'Integer' values to 'Int' values, or die trying.
    let (unOps, binOps, inputData, outputData, inColNames, outColNames, permType) = produceFindExprArgs problemInfo
    inputs  <- convertIntegersOrFail inputData
    outputs <- convertIntegersOrFail outputData

    -- Find expressions for each column of output in turn.
    let produceOneOutput o = findAllExpressions unOps binOps inputs inColNames o permType
    let allOutputs = map produceOneOutput $ transpose outputs
    let exprsToOutput = 1   -- TODO: Let user specify this value
    let firstFewOutputs = map (take exprsToOutput) allOutputs

    -- Display all of the expressions we found.
    putStr $ formatOutputs firstFewOutputs  outColNames
