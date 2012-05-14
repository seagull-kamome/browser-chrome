{-# LANGUAGE CPP #-}
module Web.Browser.Chrome.Utils where

import Control.Applicative ((<$>))
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

pathToProfileDir, pathToLocalStorageDir :: FilePath -> IO FilePath


#if !defined(mingw32_HOST_OS) && !defined(cygwin32_HOST_OS) && !defined(_WIN32)
pathToProfileDir profile = (</> ".config" </> "google-chrome" </> profile) <$> getHomeDirectory
#else
pathToProfileDir profile = (</> "Local Settings" </> "Application Data" </> "Google" </> "Chrome" </> "User Data" </> profile) <$> getHomeDirectory
#endif


pathToLocalStorageDir profile = (</> "Local Storage") <$> pathToProfileDir profile

