
import           Test.Tasty

import           Test.ErrorParsing   (huErrPar, qcErrPar)
import           Test.GenericParsing (huGenPar, qcGenPar)



main :: IO ()
main = defaultMain tests

tests       ::  TestTree
tests       =   testGroup "Tests"       [ properties, unitTests ]

properties  ::  TestTree
properties  =   testGroup "Properties"  [ qcProps ]

qcProps     ::  TestTree
qcProps     =   testGroup "QuickCheck"  [ qcErrPar
                                        , qcGenPar ]



unitTests   ::  TestTree
unitTests   =   testGroup "Unit tests"  [ hUnit ]

hUnit       ::  TestTree
hUnit       =   testGroup "HUnit"       [ huErrPar
                                        , huGenPar ]

