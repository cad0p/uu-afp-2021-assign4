
module Test.ErrorParsing (qcErrPar, huErrPar) where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Assign4.ErrorParsing

import           Data.Char             (digitToInt, isDigit)



qcErrPar :: TestTree
qcErrPar = testGroup "ErrPar" [ qcFunParser ]

huErrPar :: TestTree
huErrPar = testGroup "ErrPar" [ huIntParser
                              , huFunParser ]


huIntParser :: TestTree
huIntParser = testGroup "instance Parser Int"
  [ testCase "101" (
      runParser intSumParser "101"
    @?=
      Right (2, "")
  )]


huFunParser :: TestTree
huFunParser = testGroup "instance Functor Parser"
  [ testCase "101 (+ 1)" (
      runParser (fmap (+ 1) intSumParser) "101"
    @?=
      Right (3, "")
  )
  , testCase "101 (* 3)" (
      runParser (fmap (* 3) intSumParser) "101"
    @?=
      Right (6, "")
  )
  ]

qcFunParser :: TestTree
qcFunParser = testGroup "instance Functor Parser" []
  -- [ QC.testProperty ""]


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



