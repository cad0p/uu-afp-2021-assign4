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
                                        , huStringUntilParser ]



huBoolParser :: TestTree
huBoolParser = testGroup "parseBool"
  [ testCase "parseBool 'not a bool'" (
      parseBool "not a bool"
    @?=
      Left (ErrorMsg "couldn't parse the Bool 'not a bool'")
  )
  , testCase "parseBool 'True'" (
      parseBool "True"
    @?=
      Right (True, "")
  )]


huStringUntilParser :: TestTree
huStringUntilParser = testGroup "parseStringUntil"
  [ testCase "space terminator" (
      testCaseSpaceTerm
    @?=
      Right ("Pizza", " Hut")
  )]


testCaseSpaceTerm :: Parsed String
testCaseSpaceTerm = parseStringUntil fTerm "Pizza Hut" where
  fTerm "" = False
  fTerm (x:_) = case x of
    ' ' -> True
    _   -> False
