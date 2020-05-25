{-# LANGUAGE OverloadedStrings #-}

module Libkst.Html (
  SVG,
  encodeSVG,
) where

import Data.Text     (Text)
import Libkst.Encode

type SVG = Text

-- | Encode
encodeSVG
  :: SVG  -- ^ SVG picture
  -> Text -- ^ Base64 encoding of image
encodeSVG svg = "data:image/svg+xml;base64," <> encodeBase64 svg
