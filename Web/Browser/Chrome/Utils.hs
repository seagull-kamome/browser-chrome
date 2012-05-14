{-# LANGUAGE CPP #-}
module Web.Browser.Chrome.Utils where


import System.FilePath ((</>))

pathToProfileDir :: FilePath -> FilePath
#if !defined(mingw32_HOST_OS) && !defined(cygwin32_HOST_OS) && !defined(_WIN32)
pathToProfileDir profile = ".config" </> "google-chrome" </> profile
#else
pathToProfileDir profile = "Local Settings" </> "Application Data" </> "Google" </> "Chrome" </> "User Data" </> profile
#endif


