

module Assign4.ErrorParsing where
import           Control.Arrow (first)
import           Data.Char     (digitToInt, isDigit)


newtype ErrorMsg
  = ErrorMsg String

type ParserF a = String -> Either ErrorMsg (a, String)

{-|
  A parser consists of a function that reads from a 'String'
  to produce either an error message or a result of type a
  and the remaining String that has not been parsed.

  This parser type does not allow backtracking and is less expressive than the list-based parsers.
-}
newtype Parser a
  = Parser (ParserF a)


instance Show ErrorMsg where
  show (ErrorMsg e) = "ERROR: " ++ show e


instance Functor Parser where
  fmap f (Parser p) = Parser ((first f <$>) . p)


{-|
  >>> intSumParser "101"
  >>  Right (2,"")
-}
intSumParserF :: ParserF Int
intSumParserF "" = Left (ErrorMsg "the string is empty")
intSumParserF s = intSumParser' (0, s) where
  intSumParser' state = case state of
    (_, "")   -> Right state
    (i, x:xs) -> if isDigit x
      then intSumParser' (i + digitToInt x, xs)
      else Left (ErrorMsg "the char is not an Int")

intSumParser :: Parser Int
intSumParser = Parser intSumParserF

runParser :: Parser a -> ParserF a
runParser (Parser p) = p
