{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}
module Assign4.GenericParsing where

import           Assign4.ErrorParsing
    ( ErrorMsg (ErrorMsg)
    , Parser (parse)
    , ParserF
    )
import           Prelude              hiding (Bool (..))

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



-- Step 2: build pattern functors and combinators

{-| Either a constructor or the other -}
data (:+:) f g r
  = L (f r)
  | R (g r)

{-| Merge two constructors to create a longer one -}
data (:*:) f g r
  = f r :*: g r

{-| A constructor of the same type, recursion -}
data I r
  = I r

{-| A constant -}
data K a r
  = K a

{-| The unit -}
data U r = U

{-| see slide 26 -}
type IntTreeS = K Int :+: (I :*: I)

type BoolS = K Bool

-- type IntTreeFixityS

type NumberS = K Int


-- Step 3: define a functions to and from to convert values between user-defined datatypes and their representations

-- from :: Number -> Int
-- from (Number n) = n

-- to :: Int -> Number
-- to i = Number { n = i }

from :: Number -> NumberS a
from (Number n) = K n

to :: NumberS a -> Number
to (K i) = Number { n = i }

-- Step 4: define the generic function by induction on the structure of the representation


class Parse f where
  gparse :: String -> (String -> a -> Either ErrorMsg (a, String)) -> f a -> Either ErrorMsg (a, String)

instance Parse U where
  gparse _ f U = Left (ErrorMsg "unit")

instance Parse I where
  gparse s f (I r) = f s r

instance Parse (K Int) where
  gparse s f (K a) = f s undefined
