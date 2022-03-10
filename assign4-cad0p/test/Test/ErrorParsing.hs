
module Test.ErrorParsing (qcErrPar, huErrPar) where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Assign4.ErrorParsing

import           Data.Char             (digitToInt, isDigit)



qcErrPar :: TestTree
qcErrPar = testGroup "Error Parsing"  [ qcFunParser
                                      , qcAppParser ]

huErrPar :: TestTree
huErrPar = testGroup "Error Parsing"  [ huIntParser
                                      , huFunParser
                                      , huAppParser ]


huIntParser :: TestTree
huIntParser = testGroup "instance Parser Int"
  [ testCase "intSumParser: 101" (
      parse intSumParser "101"
    @?=
      Right (2, "")
  )]


huFunParser :: TestTree
huFunParser = testGroup "instance Functor Parser"
  [ testCase "fmap intSumParser: 101 (+ 1)" (
      parse (fmap (+ 1) intSumParser) "101"
    @?=
      Right (3, "")
  )
  , testCase "fmap intSumParser: 101 (* 3)" (
      parse (fmap (* 3) intSumParser) "101"
    @?=
      Right (6, "")
  )
  ]

huAppParser :: TestTree
huAppParser = testGroup "instance Applicative Parser"
  [ testCase "<*> (++1) 0" (
      parse (Parser (\x -> Right ((++ "1"), x)) <*> pure "0") "1234"
    @?=
      Right ("01","1234")
  )]

qcFunParser :: TestTree
qcFunParser = testGroup "instance Functor Parser"
  [ QC.testProperty "fmap intSumParser" prop_FunParserFmapIntSum ]

qcAppParser :: TestTree
qcAppParser = testGroup "instance Applicative Parser"
  [ QC.testProperty "pure" prop_AppParserPure ]


{-|
  instead of (Int -> Int), Fun Int Int
  insteaf of f x,          applyFun f x

  https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/Test-QuickCheck-Function.html#t:Fun
-}
prop_FunParserFmapIntSum :: Fun Int Int -> String -> Bool
prop_FunParserFmapIntSum f "" =
    parse (applyFun f <$> intSumParser) ""
  ==
    Left (ErrorMsg "the string is empty")
prop_FunParserFmapIntSum f s
  | all isDigit s =
      parse (applyFun f <$> intSumParser) s
    ==
      do
        (i, x) <- parse intSumParser s
        Right (applyFun f i, x)
  | otherwise =
      parse (applyFun f <$> intSumParser) s
    ==
      Left (ErrorMsg "the char is not an Int")


prop_AppParserPure :: Int -> String -> Bool
prop_AppParserPure a s =
    parse (pure a) s
  ==
    Right (a, s)


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



