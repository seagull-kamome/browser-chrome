{-
-}
module Web.Browser.Internal (Request, CookieLoader) where

import Network.HTTP.Conduit (Cookie(..))
import Network.HTTP.Types (Ascii)

class Request req where
  host :: req -> Ascii
  path :: req -> Ascii
  secure :: req -> Bool


type CookieLoader m
    = Ascii       -- Host name
    -> Ascii      -- Path to resource
    -> Bool       -- Is secure access?
    -> Bool       -- Is http access?
    -> m [Cookie]



