{-|
Module      : ParseInput
Description : Parse a description of the problem to solve
Copyright   : Scott Pakin, 2014
License     : GPL-3
Maintainer  : scott-fexpr@pakin.org
Stability   : experimental
Portability : portable

Convert textual input of the form

@
unary ops: /u/&#x2081; /u/&#x2082; &#x2026;

binary ops: /b/&#x2081; /b/&#x2082; &#x2026;

constants: /c/&#x2081; /c/&#x2082; &#x2026;

solutions: /s/

columns: /i/&#x2081; /i/&#x2082; &#x2026; => /o/&#x2081; /o/&#x2082; &#x2026;

/i/&#x2081; /i/&#x2082; &#x2026; => /o/&#x2081; /o/&#x2082; &#x2026;
/i/&#x2081; /i/&#x2082; &#x2026; => /o/&#x2081; /o/&#x2082; &#x2026;
/i/&#x2081; /i/&#x2082; &#x2026; => /o/&#x2081; /o/&#x2082; &#x2026;
        &#x22EE;
@

to a 'ParsedInput' record.  All of the above are optional except the
final block, which represents the mapping of inputs to outputs.
-}

module ParseInput ( UnaryOperator(..)
                  , BinaryOperator(..)
                  , ParsedInput(..)
                  , entireInput
                  , Permutation(..)
                  ) where

import Control.Monad
import Data.Bits
import Data.Char
import Data.List
import Data.Maybe
import Data.Typeable
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error

-- | Parse and discard intra-line number and operator separators:
-- spaces, tabs, and commas.
separators :: Parser ()
separators = skipMany1 $ oneOf " \t,"

-- | 'maybeMatch' is similar to 'option' but returns 'Just' /result/\
-- on a match and 'Nothing' on a failure to match.
maybeMatch :: Parser a -> Parser (Maybe a)
maybeMatch parser = option Nothing $ do { r <- parser ; return (Just r) }

---------------------------------------------------------------------------

-- | A 'UnaryOperator' represents the name of a unary function, a
-- unary function on 'Maybe' 'Int' (because it's allowed to fail) and
-- as a unary function on 'String' that construct a textual
-- description of an expression.
newtype UnaryOperator = UnaryOperator (String, Maybe Int -> Maybe Int, String -> String)

-- Show a 'UnaryOperator' by returning its name and ignoring the two
-- function terms.
instance Show UnaryOperator where
  show (UnaryOperator (name, _, _)) = name

-- | Associate names with unary functions.
unaryTable :: [UnaryOperator]
unaryTable =
  map UnaryOperator [("-",   liftM negate, stringPrefix "-"),
                     ("abs", liftM abs,    stringPrefix "abs"),
                     ("sgn", liftM signum, stringPrefix "sgn"),
                     ("not", not1Bit,      stringPrefix "not")]
  where stringPrefix p s = p ++ "(" ++ s ++ ")"
        not1Bit (Just 0) = Just 1
        not1Bit (Just 1) = Just 0
        not1Bit _        = Nothing

-- | Given a 'UnaryOperator' and a parser that produces a
-- 'UnaryOperator', return a new parser that matches the name of the
-- 'UnaryOperator' and, failing that, falls back onto the given
-- parser.
matchUnOperator :: UnaryOperator -> Parser UnaryOperator -> Parser UnaryOperator
matchUnOperator op@(UnaryOperator (name, ifunc, sfunc)) rhs =
          do { try $ string name
             ; return op
             } <|> rhs

-- | Parse the name of a unary operator into a 'UnaryOperator'.
unaryOp :: Parser UnaryOperator
unaryOp = foldr matchUnOperator pzero unaryTable

-- | Parse a list of space-separated unary operators beginning with
-- \"@unary ops:@\".
unaries :: Parser [UnaryOperator]
unaries = do { string "unary"
             ; space
             ; string "ops:"
             ; skipMany separators
             ; ulist <- sepEndBy unaryOp separators
             ; newline
             ; return ulist
             }

---------------------------------------------------------------------------

-- | A 'BinaryOperator' represents the name of a binary function, a
-- binary function on 'Maybe' 'Int' (because it's allowed to fail) and
-- as a binary function on 'String' that construct a textual
-- description of an expression.
newtype BinaryOperator =
  BinaryOperator (String,
                  Maybe Int -> Maybe Int -> Maybe Int,
                  String -> String -> String)

-- | Show a 'BinaryOperator' by returning its name and ignoring the two
-- function terms.
instance Show BinaryOperator where
  show (BinaryOperator (name, _, _)) = name

-- | Associate names with binary functions.
binaryTable :: [BinaryOperator]
binaryTable =
  map BinaryOperator [("+",   liftM2 (+),     stringOp "+"),
                      ("*",   liftM2 (*),     stringOp "*"),
                      ("-",   liftM2 (-),     stringOp "-"),
                      ("/",   safeDiv div,    stringOp "/"),
                      ("^",   safePower,      stringOp "^"),
                      ("mod", safeDiv mod,    stringOp "mod"),
                      ("max", liftM2 max,     stringPrefix "max"),
                      ("min", liftM2 min,     stringPrefix "min"),
                      ("and", safeBool (.&.), stringOp "and"),
                      ("or",  safeBool (.|.), stringOp "or"),
                      ("xor", safeBool xor,   stringOp "xor")]
  where stringOp opStr a b = "(" ++ a ++ ") " ++ opStr ++ " (" ++ b ++ ")"
        stringPrefix opStr a b = opStr ++ "(" ++ a ++ ", " ++ b ++ ")"
        safeDiv op (Just a) (Just b) | b /= 0 = Just (a `op` b)
        safeDiv _ _ _ = Nothing
        safeBool op (Just a) (Just b) | a >= 0 && b >= 0 = Just (a `op` b)
        safeBool _ _ _                                   = Nothing
        safePower (Just base) (Just exp) | exp < 0   = Nothing
                                         | otherwise = Just $ base^exp
        safePower _           _                      = Nothing

-- | Given a 'BinaryOperator' and a parser that produces a
-- 'BinaryOperator', return a new parser that matches the name of the
-- 'BinaryOperator' and, failing that, falls back onto the given
-- parser.
matchBinOperator :: BinaryOperator -> Parser BinaryOperator -> Parser BinaryOperator
matchBinOperator op@(BinaryOperator (name, ifunc, sfunc)) rhs =
          do { try $ string name
             ; return op
             } <|> rhs

-- | Parse the name of a binary operator into a 'BinaryOperator'.
binaryOp :: Parser BinaryOperator
binaryOp = foldr matchBinOperator pzero binaryTable

-- | Parse a list of space-separated binary operators beginning with
-- \"@binary ops:@\".
binaries :: Parser [BinaryOperator]
binaries = do { string "binary"
              ; space
              ; string "ops:"
              ; skipMany separators
              ; blist <- sepEndBy binaryOp separators
              ; newline
              ; return blist
              }

---------------------------------------------------------------------------

-- | Specify different forms of permutations to generate.  For
-- example, given @[\'A\', \'B\', \'C\']@, 'Repeated' represents
-- @[\"A\", \"B\", \"C\", \"AA\", \"AB\", \"AC\", \"BA\", \"BB\",
-- \"BC\", \"CA\", \"CB\", \"CC\", &#x2026;]@ (continuing forever);
-- 'Unique' represents @[\"A\", \"B\", \"C\", \"AB\", \"BA\", \"CB\",
-- \"BC\", \"CA\", \"AC\", \"ABC\", \"BAC\", \"CBA\", \"BCA\",
-- \"CAB\", \"ACB\"]@; and 'UniqueAllPresent' represents only
-- @[\"ABC\", \"BAC\", \"CBA\", \"BCA\", \"CAB\", \"ACB\"]@.
data Permutation = Repeated           -- ^ Values are allowed to repeat
                 | Unique             -- ^ Values are not allowed to repeat
                 | RepeatedAllPresent -- ^ Same as 'Repeated' but all values must be present
                 | UniqueAllPresent   -- ^ Same as 'Unique' but all values must be present
                 deriving Eq

-- | Parse \"@allow reps:@\" followed by either \"@yes@\" or \"@no@\".
repeatedVals :: Parser Bool
repeatedVals = do { string "allow reps:"
                  ; skipMany separators
                  ; do { string "yes"
                       ; return True
                       }
                    <|>
                    do { string "no"
                       ; return False
                       }
                  }

-- | Parse \"@require all:@\" followed by either \"@yes@\" or \"@no@\".
allPresent :: Parser Bool
allPresent = do { string "require all:"
                ; skipMany separators
                ; do { string "yes"
                     ; return True
                     }
                  <|>
                  do { string "no"
                     ; return False
                     }
                }

-- | Parse a list of space-separated integer constants beginning with
-- \"@constants:@\".
constants :: Parser [Integer]
constants = do { string "constants:"
               ; skipMany separators
               ; consts <- sepEndBy integer separators
               ; newline
               ; return consts
               }

-- | Parse a single integer beginning with
-- \"@solutions:@\".
solutionsWanted :: Parser Integer
solutionsWanted = do { string "solutions:"
                     ; skipMany separators
                     ; oneConst <- integer
                     ; skipMany separators
                     ; newline
                     ; return oneConst
                     }

-- | Parse a single character that is valid within a column name.
colChar :: Parser Char
colChar = alphaNum <|> oneOf "!@#$%^&*()-_+[]{}|:;<>?./'" <|>
          do { char '\\' ; oneOf " =\"\\," }

-- | Parse an unquoted column name.  Spaces need to be escaped.
unquotedColName :: Parser String
unquotedColName = many1 $ colChar

-- | Parse a column name between double quotes.  Spaces and commas do
-- not need to be escaped here.
quotedColName :: Parser String
quotedColName = between (char '"') (char '"') (many1 qChars)
  where qChars = colChar <|> oneOf " ,"

-- | Parse either a quoted or an unquoted column name.
colName :: Parser String
colName = unquotedColName <|> quotedColName

-- | Return an infinite list of input-column names and an infinite
-- list of output-column names.
allColumnNames :: ([String], [String])
allColumnNames = (allInputNames, allOutputNames)
  where alphabet = transpose [['a'..'z']]
        allInputNames = alphabet ++ [before ++ c |
                                     before <- allInputNames,
                                     c <- alphabet]
        allOutputNames = map (\n -> "f" ++ show n) [1..]

-- | Parse one row of inputs and output column names of the form
-- \"@columns:@ /i1/ /i2/ /i3/ &#x2026; @=>@ /o1/ /o2/ /o3/ &#x2026;\".
columnNames :: Parser ([String], [String])
columnNames = do { string "columns:"
                 ; skipMany space
                 ; inputs <- sepEndBy1 colName separators
                 ; string "=>"
                 ; skipMany separators
                 ; outputs <- sepEndBy1 colName separators
                 ; return (inputs, outputs)
                 }

-- | Parse one row of inputs and outputs of the form \"/i1/ /i2/ /i3/
-- &#x2026; @=>@ /o1/ /o2/ /o3/ &#x2026;\".
ioRow :: Parser ([Integer], [Integer])
ioRow = do { skipMany space
           ; inputs <- sepEndBy1 integer separators
           ; string "=>"
           ; skipMany separators
           ; outputs <- sepEndBy1 integer separators
           ; return (inputs, outputs)
           }

-- | Parse one or more rows of mappings from inputs to outputs.
inputsOutputs :: Parser [([Integer], [Integer])]
inputsOutputs = sepEndBy ioRow newline

---------------------------------------------------------------------------

-- The following code was adapted from Text/Parsec/Token.hs to accept
-- C-style instead of Haskell-style integers, to include support for
-- binary numbers (using C++14 syntax), and, most importantly, not to
-- discard spaces, as that messes up the preceding rules.

-- | Parse a base-16, base-10, base-8, or base-2 integer.
integer :: Parser Integer
integer = do { f <- sign
             ; n <- nat
             ; return (f n)
             }

-- | Parse a plus or minus sign into a unary function.
sign :: Parser (Integer -> Integer)
sign = (char '-' >> return negate)
       <|> (char '+' >> return id)
       <|> return id

-- | Parse a natural number.
nat :: Parser Integer
nat = zeroNumber <|> decimal

-- | Handle specially numbers starting with @0@, as this indicates
-- that a base other than 10 is being used.
zeroNumber :: Parser Integer
zeroNumber = do { char '0'
                ; hexadecimal <|> octal <|> binary <|> return 0
                }
             <?> ""

-- | Parse a base-10 integer (the default).
decimal :: Parser Integer
decimal = number 10 digit

-- | Parse a base-16 integer, which begins with \"@0x@\".
hexadecimal :: Parser Integer
hexadecimal = do { oneOf "xX"; number 16 hexDigit }

-- | Parse a base-8 integer, which begins with \"@0@\".
octal :: Parser Integer
octal = do { number 8 octDigit  }

-- | Parse a base-2 integer, which begins with \"@0b@\".
binary :: Parser Integer
binary = do { oneOf "bB" ; number 2 binDigit }

-- | Parse a bit (either @0@ or @1@).
binDigit :: Parser Char
binDigit = oneOf "01"

-- | Parse a number, given the base to use and a parser that accepts a
-- sequence of digits in the given base.
number :: Integer -> Parser Char -> Parser Integer
number base baseDigit =
  do { digits <- many1 baseDigit
     ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
     ; seq n (return n)
     }

---------------------------------------------------------------------------

-- | The 'entireInput' parser converts a string into a 'ParsedInput'
-- for convenient access.
data ParsedInput = ParsedInput { unaryFuncs  :: [UnaryOperator]          -- ^ List of unary operators
                               , binaryFuncs :: [BinaryOperator]         -- ^ List of binary operators
                               , permType    :: Permutation              -- ^ How input columns and column names should be permuted
                               , constVals   :: [Integer]                -- ^ List of constants to append to each column of inputs and input names
                               , numSolns    :: Integer                  -- ^ Number of solutions to output
                               , colNames    :: ([String], [String])     -- ^ Lists of input column names and output column names
                               , dataTable   :: [([Integer], [Integer])] -- ^ Rows of data table, each mapping inputs to outputs
                               }

-- | Output a 'ParsedInput' in a format that can be used as program input.
instance Show ParsedInput where
  show p = "unary ops: " ++ showList (unaryFuncs p) ++ "\n" ++
           "binary ops: " ++ showList (binaryFuncs p) ++ "\n" ++
           "constants: " ++ showList (constVals p) ++ "\n" ++
           "allow reps: " ++ (if reps then "yes" else "no") ++ "\n" ++
           "require all: " ++ (if reqAll then "yes" else "no") ++ "\n" ++
           "solutions: " ++ (show $ numSolns p) ++ "\n" ++
           "columns: " ++ (filter (\c -> c /= '"') $ showInsOuts $ colNames p) ++
           concatMap showInsOuts (dataTable p)
    where perm = permType p
          reps = perm == Repeated || perm == RepeatedAllPresent
          reqAll = perm == UniqueAllPresent || perm == RepeatedAllPresent
          showInsOuts (ins, outs) = (showList ins) ++ " => " ++ (showList outs) ++ "\n"
          showList lst = intercalate " " $ map show lst

-- | The entire input to parse is expected to be of the form
--
--     * Unary operators (\"@unary ops:@ &#x2026;\", default: 'allUnaries')
--
--     * Binary operators (\"@binary ops:@ &#x2026;\", default: 'allBinaries')
--
--     * Constants (\"@constants:@ &#x2026;\", default: @1@)
--
--     * Repeated or unique permutations (\"@allow reps:@
--       @yes@|@no@\", default: 'True')
--
--     * Permutations requiring all values (\"@require all:@
--       @yes@|@no@\", default: 'False')
--
--     * Number of solutions to output (\"@solutions:@\", default: 1)
--
--     * Column names (\"@columns:@ \"/i1/ /i2/ /i3/ &#x2026; @=>@
--       /o1/ /o2/ /o3/ &#x2026;\")
--
--     * Input\/output mappings (one or more rows of \"/i1/ /i2/ /i3/
--       &#x2026; @=>@ /o1/ /o2/ /o3/ &#x2026;\")
entireInput :: Parser ParsedInput
entireInput = do { skipMany space
                 ; u <- option unaryTable unaries
                 ; skipMany space
                 ; b <- option binaryTable binaries
                 ; skipMany space
                 ; c <- maybeMatch $ try constants
                 ; skipMany space
                 ; r <- option True repeatedVals
                 ; skipMany space
                 ; a <- option False allPresent
                 ; skipMany space
                 ; s <- option 1 solutionsWanted
                 ; skipMany space
                 ; n <- maybeMatch $ try columnNames
                 ; skipMany space
                 ; io <- inputsOutputs
                 ; skipMany space
                 ; eof
                 ; case validateInputs u b c r a s n io of
                   (Right parsing) -> return parsing
                   (Left errMsg)   -> fail errMsg
                 }

-- | Return whether all lists in a given list of lists have the same length.
sameLength :: [[a]] -> Bool
sameLength someList = all (\lst -> (length lst) == firstLen) someList
  where firstLen = length $ head someList

-- | Provide default column names if necessary, and truncate the
-- result to a given length.
provideColumnNames :: Int      -- ^ Number of input columns
                   -> Int      -- ^ Number of output columns
                   -> Maybe ([String], [String])  -- ^ Either a list of input and output column names or 'Nothing' for defaults
                   -> ([String], [String])  -- ^ Names of input and output columns
provideColumnNames numIn numOut given = (iNames, oNames)
  where iNames = take numIn $ iNameList
        oNames = take numOut $ oNameList
        (iNameList, oNameList) = case given of Nothing -> allColumnNames
                                               Just cNames -> cNames

-- | Postprocess and package up a set of parsed values into a
-- 'ParsedInput' if the values are valid or an error message if not.
validateInputs :: [UnaryOperator]             -- ^ Subset of unary operators to consider
               -> [BinaryOperator]            -- ^ Subset of binary operators to consider
               -> Maybe [Integer]             -- ^ List of constants, if provided
               -> Bool                        -- ^ 'True' = allow variables to be used more than once
               -> Bool                        -- ^ 'True' = require variables to be used at least once
               -> Integer                     -- ^ Number of expressions to output per column
               -> Maybe ([String], [String])  -- ^ List of input and output column names, if provided
               -> [([Integer], [Integer])]    -- ^ List of pairs associating inputs with outputs
               -> Either String ParsedInput   -- ^ A 'ParsedInput' if the above are valid or an error message if invalid
validateInputs uFuncs bFuncs cVals reps all solns cNames dTable =
  do { failIf (not $ sameLength inputRows) "Input rows have differing numbers of columns"
     ; failIf (not $ sameLength outputRows) "Output rows have differing numbers of columns"
     ; failIf (length inputColNames /= numInputs) "Not every column of input was given a name"
     ; failIf (length outputColNames /= numOutputs) "Not every column of output was given a name"
     ; failIf (solns <= 0) "The number of solutions requested must be a positive number"
     ; return $ ParsedInput {unaryFuncs  = uFuncs,
                             binaryFuncs = bFuncs,
                             constVals   = constantVals,
                             permType    = valueUsage,
                             numSolns    = solns,
                             colNames    = (inputColNames ++ constColNames, outputColNames),
                             dataTable   = dataVals}
     }
  where failIf cond msg = if cond then Left msg else return ()
        (inputRows, outputRows) = (map fst dTable, map snd dTable)
        numInputs = length $ head inputRows
        numOutputs = length $ head outputRows
        (inputColNames, outputColNames) = provideColumnNames numInputs numOutputs cNames
        valueUsage = case (reps, all) of (False, False) -> Unique
                                         (False, True)  -> UniqueAllPresent
                                         (True,  False) -> Repeated
                                         (True,  True)  -> RepeatedAllPresent
        constantVals = case cVals of Nothing -> [1]
                                     Just nums -> nums
        constColNames = map show constantVals
        dataVals = map (\(is, os) -> (is ++ constantVals, os)) dTable
