{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_CommonResources (
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

bindir     = "/home/ggunn/CS7009/CommonResources/.stack-work/install/x86_64-linux/lts-7.13/8.0.1/bin"
libdir     = "/home/ggunn/CS7009/CommonResources/.stack-work/install/x86_64-linux/lts-7.13/8.0.1/lib/x86_64-linux-ghc-8.0.1/CommonResources-0.1.0.0-BsMSFzQQ7hXDT6ERpBbAsv"
dynlibdir  = "/home/ggunn/CS7009/CommonResources/.stack-work/install/x86_64-linux/lts-7.13/8.0.1/lib/x86_64-linux-ghc-8.0.1"
datadir    = "/home/ggunn/CS7009/CommonResources/.stack-work/install/x86_64-linux/lts-7.13/8.0.1/share/x86_64-linux-ghc-8.0.1/CommonResources-0.1.0.0"
libexecdir = "/home/ggunn/CS7009/CommonResources/.stack-work/install/x86_64-linux/lts-7.13/8.0.1/libexec"
sysconfdir = "/home/ggunn/CS7009/CommonResources/.stack-work/install/x86_64-linux/lts-7.13/8.0.1/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "CommonResources_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "CommonResources_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "CommonResources_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "CommonResources_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "CommonResources_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "CommonResources_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
