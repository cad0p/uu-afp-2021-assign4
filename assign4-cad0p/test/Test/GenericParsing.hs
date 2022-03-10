module Test.GenericParsing (qcGenPar, huGenPar) where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Assign4.GenericParsing



qcGenPar :: TestTree
qcGenPar = testGroup "GenPar" []

huGenPar :: TestTree
huGenPar = testGroup "GenPar" []
