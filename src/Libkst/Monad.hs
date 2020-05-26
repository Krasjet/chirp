module Libkst.Monad (
  whenM,
  eitherToMaybe,
) where

import Control.Monad (when)

-- | A monadic version of 'when'.
whenM
  :: Monad m
  => m Bool  -- ^ Monadic condition
  -> m ()    -- ^ Action
  -> m ()
whenM condm action = condm >>= flip when action
{-# INLINE whenM #-}

-- | Either to maybe
eitherToMaybe
  :: Either e a
  -> Maybe a
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right x) = Just x
{-# INLINE eitherToMaybe #-}
