{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_raycaster (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/bart/.cabal/bin"
libdir     = "/home/bart/.cabal/lib/x86_64-linux-ghc-8.6.5/raycaster-0.1.0.0-JrGqbN9sR5J9c6xpJJOJJN-raycaster"
dynlibdir  = "/home/bart/.cabal/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/bart/.cabal/share/x86_64-linux-ghc-8.6.5/raycaster-0.1.0.0"
libexecdir = "/home/bart/.cabal/libexec/x86_64-linux-ghc-8.6.5/raycaster-0.1.0.0"
sysconfdir = "/home/bart/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "raycaster_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "raycaster_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "raycaster_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "raycaster_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "raycaster_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "raycaster_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
