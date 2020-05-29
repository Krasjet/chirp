module Libkst.IO (
  -- * FileIO
  createParentDir,
  writeFileHandleMissing,
  writeFileHandleMissing',
  writeFileHandleMissingS,
  -- * ProcessIO
  readProcessWithCWD,
  -- * Utility
  tryIODeep,
  evaluateDeep,
  orElseIO,
  orElseIO',
) where

import qualified Data.Text         as T
import qualified Data.Text.IO      as TIO
import qualified Data.Text.Lazy    as LT
import qualified Data.Text.Lazy.IO as LTIO
import qualified System.Process    as Proc

import Control.DeepSeq            (NFData, ($!!))
import Control.Exception          (IOException, catch, evaluate, try)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans.Maybe  (MaybeT (..), runMaybeT)
import System.Directory           (createDirectoryIfMissing)
import System.Exit                (ExitCode)
import System.FilePath            (takeDirectory)

-- | Create parent directory for a file.
createParentDir
  :: FilePath -- ^ A filename containing path.
  -> IO ()
createParentDir = createDirectoryIfMissing True . takeDirectory
{-# INLINEABLE createParentDir #-}

-- | Write text to a file with missing directory handling
writeFileHandleMissing
  :: FilePath -- ^ filename containing path
  -> LT.Text  -- ^ string to be written
  -> IO ()
writeFileHandleMissing f t = do
  createParentDir f
  LTIO.writeFile f t
{-# INLINEABLE writeFileHandleMissing #-}

-- | The strict version of 'writeFileHandleMissing'.
writeFileHandleMissing'
  :: FilePath -- ^ filename containing path
  -> T.Text   -- ^ string to be written
  -> IO ()
writeFileHandleMissing' f t = do
  createParentDir f
  TIO.writeFile f t
{-# INLINEABLE writeFileHandleMissing' #-}

-- | The String version of writeFileHandleMissing
writeFileHandleMissingS
  :: FilePath -- ^ filename containing path
  -> String   -- ^ string to be written
  -> IO ()
writeFileHandleMissingS f t = do
  createParentDir f
  writeFile f t
{-# INLINEABLE writeFileHandleMissingS #-}

{-
  The following functions are adapted from
  [latex-svg](https://github.com/phadej/latex-svg/blob/master/latex-svg-image/src/Image/LaTeX/Render.hs),
  2020 Oleg Grenrus.
-}

-- | Fork, execute the process and return the result.
readProcessWithCWD
  :: FilePath                      -- ^ Current working directory
  -> FilePath                      -- ^ Filename of the executable
  -> [String]                      -- ^ Arguments
  -> IO (ExitCode, String, String) -- ^ Exitcode, stdout, stderr
readProcessWithCWD cwd cmd args = Proc.readCreateProcessWithExitCode
  ((Proc.proc cmd args) { Proc.cwd = Just cwd }) ""
  --                                             ^ no stdin
{-# INLINEABLE readProcessWithCWD #-}

-- | Deep evaluate an action, catch any 'IOException's and convert them to the
-- 'ExceptT' monad
tryIODeep
  :: (MonadIO m, NFData a)
  => IO a                    -- ^ IO action.
  -> ExceptT IOException m a -- ^ Result encapsulated in exception
tryIODeep = ExceptT . liftIO . try . evaluateDeep
{-# INLINEABLE tryIODeep #-}

-- | Deep evaluate an IO action, from safe-exceptions
evaluateDeep :: NFData a => IO a -> IO a
evaluateDeep action = do
  res <- action
  evaluate $!! res
{-# INLINEABLE evaluateDeep #-}

-- | Perform action on the left. If failed, run action on the right and handle
-- any exceptions.
orElseIO
  :: IO a           -- ^ IO action that might fail.
  -> ExceptT e IO a -- ^ If action failed, perform this instead
  -> ExceptT e IO a
lft `orElseIO` rgt = ExceptT $ fmap Right lft `catch` handler rgt
  where
    handler
      :: ExceptT e IO a -- ^ action to run instead
      -> IOException    -- ^ exception from the lft ignored
      -> IO (Either e a)
    handler act _ = runExceptT act

-- | Perform IO action on the left. If failed, run action on the right and handle
-- any exceptions. This version is for the 'MaybeT' monad
orElseIO'
  :: IO a         -- ^ IO action that might fail.
  -> MaybeT IO a  -- ^ If action failed, perform this instead
  -> MaybeT IO a
lft `orElseIO'` rgt = MaybeT $ fmap Just lft `catch` handler rgt
  where
    handler
      :: MaybeT IO a -- ^ action to run instead
      -> IOException    -- ^ exception from the lft ignored
      -> IO (Maybe a)
    handler act _ = runMaybeT act
