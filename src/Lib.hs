module Lib
    ( latexDirToTex, latexToTxt
    ) where

import qualified Data.ByteString as BS
import           Data.List (isSuffixOf)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           ForeignLib (chdir)
import           Path
import           System.Directory (getDirectoryContents )
import           Text.Pandoc
import           Text.Pandoc.UTF8 (toText)

latexToTxt :: Path b File -> IO T.Text
latexToTxt  fPath = do
  fileBS <- BS.readFile $ toFilePath fPath
  result <- runIO $ do
    doc <- readLaTeX def $ toText fileBS
    writePlain def doc
  handleError result

-- | Look for documents in the directory specified (non-recursively).
getMainTexDocs :: Path b Dir -> IO [Path b File]
getMainTexDocs srcDir = do
  contents <- getDirectoryContents $ toFilePath srcDir
  let texFiles = [ f | f <- contents,  ".tex" `isSuffixOf` f ]
  texTexts <- sequence [ toText <$> BS.readFile f | f <- texFiles ]
  let texFileTexts = zip texFiles texTexts
  let texMains = [ f | (f, t) <- texFileTexts,  dclStr `T.isInfixOf` t]
  texMainRels <- traverse parseRelFile texMains
  pure $ (\f -> srcDir </> f) <$> texMainRels

  where
    dclStr = T.pack "\\documentclass"

latexDirToTex :: Path b Dir -> IO T.Text
latexDirToTex fp = do -- bracket chdir ...
  cwdNewEi <- chdir fp
  cwdNew <- case cwdNewEi of
    Right f -> pure f
    Left f -> fail $ "Couldn't chdir to " <> toFilePath fp
  mainDocs <- getMainTexDocs cwdNew
  docTxts <- traverse latexToTxt mainDocs
  pure $ T.intercalate (T.pack "\n") docTxts