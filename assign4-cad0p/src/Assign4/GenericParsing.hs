{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}
{-# HLINT ignore "Use if" #-}
module Assign4.GenericParsing where

import           Assign4.ErrorParsing
    ( ErrorMsg (ErrorMsg)
    , Parsed
    , Parser (Parser, parse)
    , ParserF
    )
import           Prelude               hiding (Bool (..))

import           Data.Char             (isDigit)
import           Text.XML.HXT.DOM.Util (decimalStringToInt)

-- Step 1: Modelling Data Types

data Fix f
  = In (f (Fix f))

data IntTreeF t
  = LeafF Int
  | NodeF t t

type IntTree = Fix IntTreeF

data Bool = True | False deriving (Eq, Show)

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
  deriving (Show)



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

type IntS = K Int


-- Step 3: define a functions to and from to convert values between user-defined datatypes and their representations

-- from :: Number -> Int
-- from (Number n) = n

-- to :: Int -> Number
-- to i = Number { n = i }

-- fromIntTree :: IntTree -> IntTreeS a
-- fromIntTree t = case t of
--   In (LeafF Int)

fromBool :: Bool -> BoolS a
fromBool = K

toBool :: BoolS a -> Bool
toBool (K b) = b

fromNumber :: Number -> NumberS a
fromNumber (Number num) = K num

toNumber :: NumberS a -> Number
toNumber (K i) = Number { n = i }

fromInt :: Int -> IntS a
fromInt = K

toInt :: IntS a -> Int
toInt (K i) = i





-- Step 4: define the generic function by induction on the structure of the representation


class Parse f where
  gparse :: (f a -> a) -> String -> Either ErrorMsg (a, String)
  gparse toType = parse (toType <$> gParser)

  gParserF :: ParserF (f a)

  gParser :: Parser (f a)
  gParser = Parser gParserF

-- instance Parse U where
--   gparse _ f U = Left (ErrorMsg "unit")

-- instance Parse I where
--   gparse s f (I r) = f s r

instance Parse (K Int) where
  gParserF = parse
    (fromInt . decimalStringToInt <$> Parser (parseStringUntil fTerm)) where

    fTerm :: String -> Bool
    fTerm "" = False
    fTerm (x:_)
      | isDigit x = False
      | otherwise = True

-- instance Parse (K String) where
--   gParserF s = Right (K s, "")


instance Parse (K Bool) where
  gParserF "True"  = Right (K True, "")
  gParserF "False" = Right (K False, "")
  gParserF s       = Left (ErrorMsg ("couldn't parse the Bool '" ++ s ++ "'"))


-- Step 5: define parsing functions

-- | Parses a Bool (True or False)
parseBool :: ParserF Bool
parseBool = gparse toBool

-- | Parses an Int
parseInt :: ParserF Int
parseInt = gparse toInt

-- | Parses a Number
parseNumber :: ParserF Number
parseNumber = gparse toNumber

{-| Parses a String until the termination condition

  The termination function could be something like
  > fTerm (x:xs) = x /= ' '

  (but not really because of the custom bool implementation)
  see here:
  https://stackoverflow.com/questions/66341296/debug-couldnt-match-expected-type-ghc-types-bool-with-actual-type-bool
-}
parseStringUntil :: (String -> Bool) -> ParserF String
parseStringUntil _ "" = Left (ErrorMsg "the string is empty")
parseStringUntil fTerm (x:xs) = parseStringUntil' (x:xs) "" where
  parseStringUntil' :: String -> String -> Parsed String
  -- if it terminates at the first iteration, it means that
  -- there is nothing to parse
  parseStringUntil' (x':xs') toParse = case fTerm (x':xs') of
    False -> parseStringUntil' xs' (toParse ++ [x'])
    True  -> case toParse of
      "" -> Left (ErrorMsg "nothing to parse")
      _  -> Right (toParse, x':xs')
  parseStringUntil' "" toParse = Right (toParse, "")
