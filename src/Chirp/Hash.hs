module Chirp.Hash (
  hashText,
  hashText'
) where

import qualified Crypto.Hash.SHA256      as SHA256
import qualified Data.ByteString.Base64  as Base64
import qualified Data.ByteString.Char8   as BS8
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as LT

-- | Create an *almost* unique identifier for a text string, which is the
-- length of the text plus its SHA256 hash. Usually used for filepath so the
-- output string is sanitized.
--
-- The chance of collision should be ignorable.
hashText :: LT.Text -> String
hashText text =
  show len <> "_" <> map sanitizePath (BS8.unpack $ Base64.encode t)
    where
      (t, len) = SHA256.hashlazyAndLength $ LT.encodeUtf8 text

      sanitizePath :: Char -> Char
      sanitizePath c
        | c == '=' || c == '/' = '-'
        | otherwise = c

-- | The strict version of 'hashText'.
hashText' :: T.Text -> String
hashText' = hashText . LT.fromStrict
{-# INLINE hashText' #-}
