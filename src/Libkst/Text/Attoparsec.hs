{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
module Libkst.Text.Attoparsec (
  double',
) where

import qualified Data.Attoparsec.Text as A
import qualified Data.Text            as T

import Control.Applicative  ((<|>))
import Data.Attoparsec.Text (Parser, (<?>))
import Data.Char            (isDigit, ord)
import Data.Text            (Text)

-- | Parse a signed rational number as 'Double'. This is different from
-- 'A.double' in that it allows leading dots, e.g. @.1234@.
--
-- Partly from latex-svg, 2020 Oleg Grenrus, with some clean up and rewritten
-- for Attoparsec.
double' :: Parser Double
double' = A.signed unsignedDouble <?> "double'"

-- * Helpers

-- | 'double'' without sign
unsignedDouble :: Parser Double
unsignedDouble =  withleadingDot <|> woleadingDot

-- | @0.01@
woleadingDot :: Parser Double
woleadingDot = do
  d <- decimal
  f <- A.option 0 withleadingDot
  return (d + f)

-- | @.01@
withleadingDot :: Parser Double
withleadingDot = A.char '.' *> fraction

-- | number before decimal point
decimal :: Parser Double
decimal = T.foldl' (\x d -> 10 * x + digitToInt d) 0 <$> digits1

-- | number after decimal point
fraction :: Parser Double
fraction = uncurry (/) .
  T.foldl' (\(!x,!n) d -> (10 * x + digitToInt d, n * 10)) (0,1)
  <$> digits1

-- | Any number of digits
digits1 :: Parser Text
digits1 = A.takeWhile1 isDigit

-- | Any number of digits
digitToInt :: Char -> Double
digitToInt c = fromIntegral $ ord c - 48
