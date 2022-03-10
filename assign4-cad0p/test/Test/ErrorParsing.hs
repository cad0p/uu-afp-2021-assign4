
module Test.ErrorParsing (qcErrPar, huErrPar) where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Assign4.ErrorParsing

import           Data.Char            (digitToInt, isDigit)



qcErrPar :: TestTree
qcErrPar = testGroup "ErrPar" []

huErrPar :: TestTree
huErrPar = testGroup "ErrPar" [ huIntParser ]


huIntParser :: TestTree
huIntParser = testGroup "parser int instance"
  [ testCase "101" (
      runParser intSumParser "101"
    @?=
      Right (2, "")
  )]


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



