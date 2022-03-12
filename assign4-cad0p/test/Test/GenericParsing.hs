module Test.GenericParsing (qcGenPar, huGenPar) where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Prelude                hiding (Bool (..))

import           Assign4.ErrorParsing
import           Assign4.GenericParsing



qcGenPar :: TestTree
qcGenPar = testGroup "Generic Parsing" []

huGenPar :: TestTree
huGenPar = testGroup "Generic Parsing"  [ huBoolParser
                                        , huStringUntilParser
                                        , huIntParser ]



huBoolParser :: TestTree
huBoolParser = testGroup "parseBool"
  [ testCase "not a bool" (
      parseBool "not a bool"
    @?=
      Left (ErrorMsg "couldn't parse the Bool 'not a bool'")
  )
  , testCase "True" (
      parseBool "True"
    @?=
      Right (True, "")
  )]


huStringUntilParser :: TestTree
huStringUntilParser = testGroup "parseStringUntil"
  [ testCase "space terminator, remaining string" (
      testCaseSpaceTerm "Pizza Hut"
    @?=
      Right ("Pizza", " Hut")
  )
  , testCase "space terminator, finished string" (
      testCaseSpaceTerm "Pizza"
    @?=
      Right ("Pizza", "")
  )]


huIntParser :: TestTree
huIntParser = testGroup "parseInt"
  [ testCase "10" (
      parseInt "10"
    @?=
      Right (10, "")
  )
  , testCase "10 fgh" (
      parseInt "10 fgh"
    @?=
      Right (10, " fgh")
  )
  , testCase "fgh" (
      parseInt "fgh"
    @?=
      Left (ErrorMsg "nothing to parse")
  )]


testCaseSpaceTerm :: String -> Parsed String
testCaseSpaceTerm = parseStringUntil fTerm where
  fTerm "" = False
  fTerm (x:_) = case x of
    ' ' -> True
    _   -> False
