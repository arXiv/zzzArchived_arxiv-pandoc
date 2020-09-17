module Lib
    ( detectEnc, latexDirToTex, latexToTxt
    ) where

import           Codec.Text.Detect
import           Codec.Text.IConv
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.List (isSuffixOf)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8With)
import qualified Data.Text.IO as TIO
import           ForeignLib (chdir)
import           Path
import           System.Directory (getDirectoryContents )
import           Text.Bytedump (dumpRawBS)
import           Text.Pandoc
import           Text.Pandoc.UTF8 (toText)
import           Data.Text.Encoding.Error (lenientDecode)

latexToTxt :: Path b File -> IO T.Text
latexToTxt  fPath = do
  fileBS <- BS.readFile $ toFilePath fPath
  result <- runIO $ do
    doc <- readLaTeX def $ toUTF8txt fileBS
    writePlain def doc
  handleError result

-- | Look for documents in the directory specified (non-recursively).
getMainTexDocs :: Path b Dir -> IO [Path b File]
getMainTexDocs srcDir = do
  contents <- getDirectoryContents $ toFilePath srcDir
  let texFiles = [ f | f <- contents,  ".tex" `isSuffixOf` f ]
  texBSs <- sequence [ BS.readFile f | f <- texFiles ]
  -- let encodings = getEnc <$> texBSs --  DEBUG
  -- putStrLn $ show $ encodings --  DEBUG
  let texTexts = toUTF8txt <$> texBSs
  let texFileTexts = zip texFiles texTexts
  let texMains = [ f | (f, t) <- texFileTexts,  dclStr `T.isInfixOf` t]
  texMainRels <- traverse parseRelFile texMains
  pure $ (\f -> srcDir </> f) <$> texMainRels
  where
    getEnc = detectEnc
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

detectEnc :: BS.ByteString -> String
detectEnc inBS = case detectEncodingName $ BSL.fromStrict inBS of
  Just enc -> enc
  Nothing -> defaultEnc

defaultEnc :: EncodingName
defaultEnc = "ISO-8859-1"

toUTF8txt :: BS.ByteString -> T.Text
toUTF8txt inBS = outTxt
  where
    encName = detectEnc inBS
    iconvBS = case encName of
      "UTF-8" -> inBS
      notUTF8enc -> BSL.toStrict $
        convertFuzzy Transliterate notUTF8enc "UTF-8" (BSL.fromStrict inBS)
    outTxt = case BS.null iconvBS of
      True -> decodeUtf8With lenientDecode inBS
      False -> toText iconvBS
