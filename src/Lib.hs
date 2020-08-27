module Lib
    ( latexToTxt
    ) where

import qualified Data.ByteString as BS
import           Data.List (isSuffixOf)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
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
  texFiles <- sequence [ toText <$> BS.readFile f
    | f <- contents,  ".tex" `isSuffixOf` f ]
  let texMains = [ T.unpack c | c <- texFiles,  dclStr `T.isInfixOf` c]
  texMainRels <- traverse parseRelFile texMains
  pure $ (\f -> srcDir </> f) <$> texMainRels

  where
    dclStr = T.pack "\\documentclass"
