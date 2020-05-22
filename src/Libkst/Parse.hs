module Libkst.Parse (
  spanL,
  spanR,
) where

import Data.List                  (isPrefixOf)

-- TODO to be replaced with real parser

{-
  The following functions comes from
  [latex-svg](https://github.com/phadej/latex-svg/blob/master/latex-svg-image/src/Image/LaTeX/Render.hs),
  2020 Oleg Grenrus.
-}

-- | Search for the first instanse of sep, for example,
-- spanL "<xml" "<p><xml" == ("<p>","<xml")
spanL :: Eq a => [a] -> [a] -> ([a], [a])
spanL sep = go where
  go str@[]                  = (str, str)
  go str@(c:sfx)
    | sep `isPrefixOf` str = ([], str)
    | otherwise            = (c:xs , ys)
    where
      ~(xs,ys) = go sfx

-- | search for the first instanse of sep, e.g.
-- spanR '>' "<xml></p>" == ("<xml>","</p>")
spanR :: Eq a => a -> [a] -> ([a], [a])
spanR sep = go where
  go  str@[]      = (str, str)
  go _str@(c:sfx)
    | sep == c  = ([c], sfx)
    | otherwise = (c:xs , ys)
    where
      ~(xs,ys) = go sfx
