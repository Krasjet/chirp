module Chirp.Monad (
  whenM,
  unlessM,
  eitherToMaybe,
) where

import Control.Monad (when, unless)

-- | A monadic version of 'when'.
whenM
  :: Monad m
  => m Bool  -- ^ Monadic condition
  -> m ()    -- ^ Action
  -> m ()
whenM condm action = condm >>= flip when action
{-# INLINE whenM #-}

-- | A monadic version of 'unless'.
unlessM
  :: Monad m
  => m Bool  -- ^ Monadic condition
  -> m ()    -- ^ Action
  -> m ()
unlessM condm action = condm >>= flip unless action
{-# INLINE unlessM #-}

-- | Either to maybe
eitherToMaybe
  :: Either e a
  -> Maybe a
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right x) = Just x
{-# INLINE eitherToMaybe #-}
