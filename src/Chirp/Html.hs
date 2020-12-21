{-# LANGUAGE OverloadedStrings #-}

module Chirp.Html (
  SVG,
  encodeSVG,
) where

import Data.Text     (Text)
import Chirp.Encode

type SVG = Text

-- | Encode
encodeSVG
  :: SVG  -- ^ SVG picture
  -> Text -- ^ Base64 encoding of image
encodeSVG svg = "data:image/svg+xml;base64," <> encodeBase64 svg
{-# INLINE encodeSVG #-}
