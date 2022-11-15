{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_rock_paper_scissors (
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
version = Version [0,1,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/deshyt/.cabal/bin"
libdir     = "/home/deshyt/.cabal/lib/x86_64-linux-ghc-9.2.5/rock-paper-scissors-0.1.0.0-inplace-rock-paper-scissors"
dynlibdir  = "/home/deshyt/.cabal/lib/x86_64-linux-ghc-9.2.5"
datadir    = "/home/deshyt/.cabal/share/x86_64-linux-ghc-9.2.5/rock-paper-scissors-0.1.0.0"
libexecdir = "/home/deshyt/.cabal/libexec/x86_64-linux-ghc-9.2.5/rock-paper-scissors-0.1.0.0"
sysconfdir = "/home/deshyt/.cabal/etc"

getBinDir     = catchIO (getEnv "rock_paper_scissors_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "rock_paper_scissors_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "rock_paper_scissors_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "rock_paper_scissors_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "rock_paper_scissors_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "rock_paper_scissors_sysconfdir") (\_ -> return sysconfdir)




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
