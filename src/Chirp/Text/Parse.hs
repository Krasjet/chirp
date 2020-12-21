{-# LANGUAGE OverloadedStrings #-}
module Chirp.Text.Parse (
  skipTo,
  skipAfter,
  takeUntil,
  between,
  between',
  betweenC,
  betweenC',
) where

import qualified Data.Text as T

import Data.Text (Text)

-- | Skip anything before the first instance of prefix, used as a preprocessor
-- for parsec. Parser combinators are not very good at this due to
-- back tracking. Will return @""@ if prefix is not in the string.
--
-- @
-- skipTo "<xml>" "<p><xml></xml>" == "<xml></xml>"
-- @
skipTo
  :: Text -- ^ Prefix
  -> Text -- ^ The string
  -> Text -- ^ Text after prefix
skipTo pfx = snd . T.breakOn pfx
{-# INLINE skipTo #-}

-- | Skip after the first instance of prefix, used as a preprocessor
-- for parsec. Will return 'Nothing' if prefix is not in the string.
--
-- @
-- skipAfter "\\begin{" "aaaa\\begin{env}bbbb" == Just "env}bbbb"
-- skipAfter "\\begin{" "aaaa\\end{env}bbbb" == Nothing
-- @
skipAfter
  :: Text       -- ^ Prefix
  -> Text       -- ^ The string
  -> Maybe Text -- ^ Text after prefix, or 'Nothing' if prefix not in string
skipAfter pfx = T.stripPrefix pfx . skipTo pfx
{-# INLINE skipAfter #-}

-- | Take until the first instance of suffix is found, used as preprocessor for
-- parsec. Will return @""@ if prefix is not in the string.
--
-- @
-- takeUntil "</xml>" "<p><xml></xml>" == "<p><xml>"
-- @
takeUntil
  :: Text -- ^ Suffix
  -> Text -- ^ The string
  -> Text -- ^ Text before suffix
takeUntil sfx = fst . T.breakOn sfx
{-# INLINE takeUntil #-}

-- | Obtain the text between two delimiters. If prefix or suffix is not found, returns
-- 'Nothing'.
--
-- @
-- between "<xml>" "</xml>" "<xml>text</xml>" == Just "text"
-- between "<xml>" "</xml>" "<xml>text<xml>"  == Nothing
-- @
between
  :: Text -- ^ Begin delimiter
  -> Text -- ^ End delimiter
  -> Text -- ^ The string.
  -> Maybe Text -- ^ Text between begin and end
between beg end txt = do
  sfx <- skipAfter beg txt
  case T.breakOn end sfx of
    (_, "")  -> Nothing
    (pfx, _) -> return pfx
{-# INLINEABLE between #-}

-- | The same as 'between' except suffix delimiter is 'Char'. This might
-- be slightly more efficient than 'between'.
--
-- @
-- between "\\begin{" '}' "\\begin{env}" == Just "env"
-- between "\\begin{" '}' "\\begin{env"  == Nothing
-- @
betweenC
  :: Text       -- ^ Begin delimiter
  -> Char       -- ^ End delimiter
  -> Text       -- ^ The string.
  -> Maybe Text -- ^ Text between begin and end
betweenC beg end txt = do
  sfx <- skipAfter beg txt
  case T.break (==end) sfx of
    (_, "")  -> Nothing
    (pfx, _) -> return pfx
{-# INLINEABLE betweenC #-}

-- | The same as 'between', but if prefix is found but suffix is not found,
-- returns anything after prefix. Might be slightly more efficient.
--
-- @
-- between "<xml>" "</xml>" "<xml>text</xml>" == Just "text"
-- between "<xml>" "</xml>" "<xml>text<xml>"  == Just "text<xml>"
-- @
between'
  :: Text       -- ^ Prefix delimiter
  -> Text       -- ^ Suffix delimiter
  -> Text       -- ^ The string.
  -> Maybe Text -- ^ Text between begin and end
between' beg end txt = skipAfter beg txt >>= (return . takeUntil end)
{-# INLINEABLE between' #-}

-- | The same as 'between'' except suffix delimiter is a character. This might
-- be slightly more efficient than 'between''.
--
-- @
-- between "\\begin{" '}' "\\begin{env}" == Just "env"
-- between "\\begin{" '}' "\\begin{env"  == Just "env"
-- @
betweenC'
  :: Text -- ^ Prefix delimiter
  -> Char -- ^ Suffix delimiter
  -> Text -- ^ The string.
  -> Maybe Text -- ^ Text between begin and end
betweenC' beg end txt = skipAfter beg txt >>= (return . T.takeWhile (/=end))
{-# INLINEABLE betweenC' #-}
