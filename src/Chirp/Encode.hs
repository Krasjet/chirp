module Chirp.Encode (
  encodeBase64,
) where

import qualified Data.ByteString.Base64  as Base64
import qualified Data.Text.Encoding      as T

import Data.Text (Text)

-- | Obtain the Base64 encoding of text.
encodeBase64
  :: Text -- ^ Text
  -> Text -- ^ Base64 Encoding
encodeBase64 = T.decodeUtf8 . Base64.encode . T.encodeUtf8
{-# INLINE encodeBase64 #-}
