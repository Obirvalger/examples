module Paths_liqhs (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/obirvalger/prog/examples/haskell/liqhs/.stack-work/install/x86_64-linux-ncurses6/lts-6.30/7.10.3/bin"
libdir     = "/home/obirvalger/prog/examples/haskell/liqhs/.stack-work/install/x86_64-linux-ncurses6/lts-6.30/7.10.3/lib/x86_64-linux-ghc-7.10.3/liqhs-0.1.0.0-K8XP1K7BrOd9z9OpeJ2gUk"
datadir    = "/home/obirvalger/prog/examples/haskell/liqhs/.stack-work/install/x86_64-linux-ncurses6/lts-6.30/7.10.3/share/x86_64-linux-ghc-7.10.3/liqhs-0.1.0.0"
libexecdir = "/home/obirvalger/prog/examples/haskell/liqhs/.stack-work/install/x86_64-linux-ncurses6/lts-6.30/7.10.3/libexec"
sysconfdir = "/home/obirvalger/prog/examples/haskell/liqhs/.stack-work/install/x86_64-linux-ncurses6/lts-6.30/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "liqhs_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "liqhs_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "liqhs_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "liqhs_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "liqhs_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
