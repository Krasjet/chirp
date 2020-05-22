module Libkst.Monad (
  whenM
) where

import Control.Monad (when)

-- | A monadic version of 'when'.
whenM
  :: Monad m
  => m Bool  -- ^ Monadic condition
  -> m ()    -- ^ Action
  -> m ()
whenM condm action = condm >>= flip when action
