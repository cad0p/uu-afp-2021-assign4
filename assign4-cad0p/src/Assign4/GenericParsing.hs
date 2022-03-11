{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Assign4.GenericParsing where

import           Assign4.ErrorParsing (Parser (parse))

-- Step 1: Modelling Data Types

data Fix f
  = In (f (Fix f))

data IntTreeF t
  = Leaf Int
  | Node t t

type IntTree = Fix IntTreeF

data Bool = True | False

data Number
  = Number
      { n :: Int
      }

-- Step 2: define a functions to and from to convert values between user-defined datatypes and their representations


-- Step 3: define the generic function by induction on the structure of the representation
