module Paths_commenter (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0,0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/lbh/commenter/cabal-dev//bin"
libdir     = "/home/lbh/commenter/cabal-dev//lib/commenter-0.0.0.1/ghc-7.4.1"
datadir    = "/home/lbh/commenter/cabal-dev//share/commenter-0.0.0.1"
libexecdir = "/home/lbh/commenter/cabal-dev//libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "commenter_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "commenter_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "commenter_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "commenter_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
