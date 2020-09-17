module Main where

import Control.Monad.Catch (MonadThrow)
import Data.List (isPrefixOf)
import Data.Maybe (listToMaybe)
import Lib
import Lib (magicInit)
import Path
import Path.IO
import System.Environment (getArgs)
import Turtle hiding (FilePath)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  argMay <- listToMaybe <$> getArgs
  srcDir <- case argMay of
    Just sd -> autoResolveDir sd
    Nothing -> getCurrentDir
  magic <- magicInit
  cwd <- pwd
  txt <- latexDirToTex magic srcDir
  TIO.putStrLn txt


autoResolveDir :: (MonadIO m, MonadThrow m)
  => FilePath          -- ^ Path to resolve
  -> m (Path Abs Dir)
autoResolveDir fp = case isRel fp of
  True -> resolveDir' fp
  False -> parseAbsDir fp

isRel :: FilePath -> Bool
isRel fp = "." `isPrefixOf` fp || (not $ "/" `isPrefixOf` fp)