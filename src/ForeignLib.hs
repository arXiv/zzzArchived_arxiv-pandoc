{-# LANGUAGE ForeignFunctionInterface #-}

module ForeignLib (chdir) where

import           Foreign.C.String (CString, newCString)
import           Foreign.C.Types
import           Path
import           Path.IO

foreign import ccall unsafe "chdir" chdir_unsafe :: CString -> IO CInt

chdir :: Path b Dir -> IO (Either (Path Abs Dir) (Path Abs Dir))
chdir newCWD = do
  newCWDcstr <- newCString (toFilePath newCWD)
  res <- chdir_unsafe newCWDcstr
  cwd <- getCurrentDir
  pure $ case res of
    0 -> Right cwd
    _ -> Left cwd
