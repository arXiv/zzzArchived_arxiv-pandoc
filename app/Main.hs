module Main where

import Lib
import System.Environment (getArgs)
import Turtle

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = putStrLn "hello"
{- main = do
  cwd <- pwd
  txt <- latexToTxt srcDir
  TIO.putStrLn txt -}