{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_n1 (
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
bindir     = "C:\\Users\\\211mar Bessi \211marsson\\HR\\\214nn 4\\FunctionalProgramming\\EDAN40-Assignment-1\\.stack-work\\install\\724b212f\\bin"
libdir     = "C:\\Users\\\211mar Bessi \211marsson\\HR\\\214nn 4\\FunctionalProgramming\\EDAN40-Assignment-1\\.stack-work\\install\\724b212f\\lib\\x86_64-windows-ghc-9.4.8\\n1-0.1.0.0-12vCZoA6hX24YJIYPBsvAW-n1-exe"
dynlibdir  = "C:\\Users\\\211mar Bessi \211marsson\\HR\\\214nn 4\\FunctionalProgramming\\EDAN40-Assignment-1\\.stack-work\\install\\724b212f\\lib\\x86_64-windows-ghc-9.4.8"
datadir    = "C:\\Users\\\211mar Bessi \211marsson\\HR\\\214nn 4\\FunctionalProgramming\\EDAN40-Assignment-1\\.stack-work\\install\\724b212f\\share\\x86_64-windows-ghc-9.4.8\\n1-0.1.0.0"
libexecdir = "C:\\Users\\\211mar Bessi \211marsson\\HR\\\214nn 4\\FunctionalProgramming\\EDAN40-Assignment-1\\.stack-work\\install\\724b212f\\libexec\\x86_64-windows-ghc-9.4.8\\n1-0.1.0.0"
sysconfdir = "C:\\Users\\\211mar Bessi \211marsson\\HR\\\214nn 4\\FunctionalProgramming\\EDAN40-Assignment-1\\.stack-work\\install\\724b212f\\etc"

getBinDir     = catchIO (getEnv "n1_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "n1_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "n1_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "n1_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "n1_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "n1_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
