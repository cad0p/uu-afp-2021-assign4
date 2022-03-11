
{-|
Module      : Assign4.ErrorParsing
Description : Exercise 1 – Parsing with error messages
Copyright   : (c) Pier Carlo Cadoppi, 2022
License     : MIT License
Maintainer  : p.c.cadoppi@students.uu.nl
Stability   : experimental
-}

module Assign4.ErrorParsing where


import           Control.Arrow (first)
import           GHC.Base      (Alternative (..))


newtype ErrorMsg
  = ErrorMsg String

type ParserF a = String -> Either ErrorMsg (a, String)

{-|
  A parser consists of a function that reads from a 'String'
  to produce either an error message or a result of type a
  and the remaining String that has not been parsed.

  This parser type does not allow backtracking and is less expressive than the list-based parsers.
-}
newtype Parser a
  = Parser { parse :: ParserF a }


instance Show ErrorMsg where
  show (ErrorMsg e) = "ERROR: " ++ show e

instance Eq ErrorMsg where
  (ErrorMsg a) == (ErrorMsg b) = a == b


instance Functor Parser where
  fmap f (Parser p) = Parser ((first f <$>) . p)


instance Applicative Parser where
  pure a = Parser (\x -> Right (a, x))
  (Parser f) <*> p' = Parser (\x -> case f x of
    Left err       -> Left err
    -- pd parsed, tp to parse
    Right (pd, tp) -> parse (pd <$> p') tp)


instance Monad Parser where
  Parser p >>= f = Parser (
    \x -> case p x of
      Left err       -> Left err
      Right (pd, tp) -> parse (f pd) tp
    )


instance Alternative Parser where
  empty = Parser (\_ -> Left (ErrorMsg "empty"))
  (Parser p) <|> (Parser p') = Parser (\c -> case p c of
    Left _ -> p' c
    pc     -> pc)
