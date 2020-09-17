module Lib
    ( detectEnc, latexDirToTex, latexToTxt, magicInit
    ) where

import           Codec.Text.Detect
import           Codec.Text.IConv
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.List (isSuffixOf)
import qualified Data.Text as T
import           Text.Bytedump (dumpRawBS)
import qualified Data.Text.IO as TIO
import           ForeignLib (chdir)
import           Magic
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
getMainTexDocs :: Magic -> Path b Dir -> IO [Path b File]
getMainTexDocs magic srcDir = do
  contents <- getDirectoryContents $ toFilePath srcDir
  let texFiles = [ f | f <- contents,  ".tex" `isSuffixOf` f ]
  putStrLn "DEBUG 1"
  texBSs <- sequence [ BS.readFile f | f <- texFiles ]
  putStrLn "DEBUG 2"
  -- encodings <- sequence $ getEnc <$> texBSs
  let encodings = getEnc <$> texBSs
  putStrLn "DEBUG 3"
  let texTexts = toText <$> texBSs
  putStrLn $ show $ encodings --  DEBUG
  let texFileTexts = zip texFiles texTexts
  let texMains = [ f | (f, t) <- texFileTexts,  dclStr `T.isInfixOf` t]
  texMainRels <- traverse parseRelFile texMains
  pure $ (\f -> srcDir </> f) <$> texMainRels
  where
    getEnc = detectEnc-- magic
    dclStr = T.pack "\\documentclass"

latexDirToTex :: Magic -> Path b Dir -> IO T.Text
latexDirToTex magic fp = do -- bracket chdir ...
  cwdNewEi <- chdir fp
  cwdNew <- case cwdNewEi of
    Right f -> pure f
    Left f -> fail $ "Couldn't chdir to " <> toFilePath fp
  mainDocs <- getMainTexDocs magic cwdNew
  docTxts <- traverse latexToTxt mainDocs
  pure $ T.intercalate (T.pack "\n") docTxts

-- | Not thread-safe (use one per thread)
magicInit :: IO Magic
magicInit = do
  magic <- magicOpen [MagicMime]
  magicLoadDefault magic
  pure magic

detectEnc :: BS.ByteString -> String
detectEnc inBS = case detectEncodingName $ BSL.fromStrict inBS of
  Just enc -> enc
  Nothing -> defaultEnc

{- detectEncoding :: Magic -> BS.ByteString -> IO EncodingName
detectEncoding magic inBS = do
  putStrLn "DEBUG 2.1"
  magicRes <- magicString magic $ dumpRawBS inBS
  putStrLn "DEBUG 2.2"
  putStrLn $ "magicRes is: " <> magicRes -- DEBUG
  putStrLn "DEBUG 2.3"
  pure magicRes -- TODO -}

defaultEnc :: String
defaultEnc = "ISO-8859-1"
