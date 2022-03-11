{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE TypeOperators #-}
module Assign4.GenericParsing where

import           Assign4.ErrorParsing (Parser (parse))

-- Step 1: Modelling Data Types

data Fix f
  = In (f (Fix f))

data IntTreeF t
  = LeafF Int
  | NodeF t t

type IntTree = Fix IntTreeF

data Bool = True | False

-- | Bonus 1: Fixity taken into account
data IntTreeFixityF t
  = LeafFixityF Int
  | t :|: t

type IntTreeFixity = Fix IntTreeFixityF


-- | Bonus 2: Record labels taken into account
data Number
  = Number
      { n :: Int
      }



-- Step 2: define a functions to and from to convert values between user-defined datatypes and their representations

from :: Number -> Int
from (Number n) = n

to :: Int -> Number
to i = Number { n = i }

-- Step 3: define the generic function by induction on the structure of the representation

data (:+:) f g r
  = L (f r)
  | R (g r)

data (:*:) f g r
  = f r :*: g r

-- class Parse
