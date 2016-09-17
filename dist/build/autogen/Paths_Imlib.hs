module Paths_Imlib (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/lazor/.cabal/bin"
libdir     = "/home/lazor/.cabal/lib/Imlib-0.1.1/ghc-6.10.1"
datadir    = "/home/lazor/.cabal/share/Imlib-0.1.1"
libexecdir = "/home/lazor/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "Imlib_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "Imlib_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "Imlib_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "Imlib_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
