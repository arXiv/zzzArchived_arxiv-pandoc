module Lib
    ( docToTxt
    ) where

import Text.Pandoc
import qualified Data.Text as T
import qualified Data.Text.IO as TIO


docToTxt :: IO T.Text
docToTxt = do
  result <- runIO $ do
    doc <- readMarkdown def (T.pack "[testing](url)")
    writePlain def doc
  handleError result
