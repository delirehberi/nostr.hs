{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_time_compat (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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
version = Version [1,9,9] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/delirehberi/.cabal/store/ghc-9.8.2/time-compat-1.9.9-2da3c6577a66442c3a3564c337469290a8e9a05d4bf7c760cfd3a58aefa89a6a/bin"
libdir     = "/home/delirehberi/.cabal/store/ghc-9.8.2/time-compat-1.9.9-2da3c6577a66442c3a3564c337469290a8e9a05d4bf7c760cfd3a58aefa89a6a/lib"
dynlibdir  = "/home/delirehberi/.cabal/store/ghc-9.8.2/time-compat-1.9.9-2da3c6577a66442c3a3564c337469290a8e9a05d4bf7c760cfd3a58aefa89a6a/lib"
datadir    = "/home/delirehberi/.cabal/store/ghc-9.8.2/time-compat-1.9.9-2da3c6577a66442c3a3564c337469290a8e9a05d4bf7c760cfd3a58aefa89a6a/share"
libexecdir = "/home/delirehberi/.cabal/store/ghc-9.8.2/time-compat-1.9.9-2da3c6577a66442c3a3564c337469290a8e9a05d4bf7c760cfd3a58aefa89a6a/libexec"
sysconfdir = "/home/delirehberi/.cabal/store/ghc-9.8.2/time-compat-1.9.9-2da3c6577a66442c3a3564c337469290a8e9a05d4bf7c760cfd3a58aefa89a6a/etc"

getBinDir     = catchIO (getEnv "time_compat_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "time_compat_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "time_compat_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "time_compat_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "time_compat_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "time_compat_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
