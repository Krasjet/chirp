module Libkst.IO (
  -- * FileIO
  createParentDir,
  writeFileHandleMissing,
  writeFileHandleMissing',
  writeFileHandleMissingS,
  -- * ProcessIO
  readProcessWithCWD,
  -- * Utility
  tryIO,
) where

import qualified Data.Text         as T
import qualified Data.Text.IO      as TIO
import qualified Data.Text.Lazy    as LT
import qualified Data.Text.Lazy.IO as LTIO
import qualified System.Process    as Proc

import Control.DeepSeq            (NFData, ($!!))
import Control.Exception          (IOException, evaluate, try)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Monad.Trans.Except (ExceptT (..))
import System.Directory           (createDirectoryIfMissing)
import System.Exit                (ExitCode)
import System.FilePath            (takeDirectory)

-- | Create parent directory for a file.
createParentDir
  :: FilePath -- ^ A filename containing path.
  -> IO ()
createParentDir = createDirectoryIfMissing True . takeDirectory

-- | Write text to a file with missing directory handling
writeFileHandleMissing
  :: FilePath -- ^ filename containing path
  -> LT.Text  -- ^ string to be written
  -> IO ()
writeFileHandleMissing f t = do
  createParentDir f
  LTIO.writeFile f t

-- | The strict version of 'writeFileHandleMissing'.
writeFileHandleMissing'
  :: FilePath -- ^ filename containing path
  -> T.Text   -- ^ string to be written
  -> IO ()
writeFileHandleMissing' f t = do
  createParentDir f
  TIO.writeFile f t

-- | The String version of writeFileHandleMissing
writeFileHandleMissingS
  :: FilePath -- ^ filename containing path
  -> String   -- ^ string to be written
  -> IO ()
writeFileHandleMissingS f t = do
  createParentDir f
  writeFile f t

{-
  The following functions comes from
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

-- | Catch 'IOException's and convert them to the 'ExceptT' monad
tryIO
  :: (MonadIO m, NFData a)
  => IO a                      -- ^ IO action.
  -> ExceptT IOException m a -- ^ Result encapsulated in exception
tryIO action = ExceptT $ liftIO $ try $ evaluateDeep action
  where
    evaluateDeep :: NFData a => IO a -> IO a
    evaluateDeep act = do
      res <- act
      evaluate $!! res
