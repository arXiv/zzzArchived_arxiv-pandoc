module Main where

import Lib

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  txt <- docToTxt
  TIO.putStrLn txt