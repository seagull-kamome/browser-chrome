{-# LANGUAGE NamedFieldPuns, RecordWildCards, ViewPatterns,OverloadedStrings #-}
{-

doctest --optghc=-XOverloadedStrings
-}
module Web.Browser.Chrome.Cookie (
  loadCookies,
  module Web.Browser.Internal,
  ) where

import qualified Data.ByteString.UTF8 as U
import qualified Data.ByteString.Char8 as BS
import Data.List (intersperse)
import Data.Time.Clock (addUTCTime, UTCTime (UTCTime))
import Data.Time.Calendar (fromGregorian)
import qualified Database.SQLite3 as SQL
import Control.Applicative
import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import qualified Control.Monad.Trans.Resource as R
import Control.Exception (bracket)

import qualified Data.Conduit as C
import qualified Data.Conduit.List as C
import qualified Network.HTTP.Conduit as C

import Network.HTTP.Types (Ascii)
import Text.Regex (mkRegex, matchRegex)
import System.Directory (getHomeDirectory,doesFileExist)
import System.FilePath ((</>))

import Web.Browser.Internal
import Web.Browser.Chrome.Utils (pathToProfileDir)
import Database.SQLite3.Conduit (stmtSource)



{-| ホストがIPアドレスかどうかを判定する [from http-conduit]

>>> isIpAddress "www.example.com"
False
>>> isIpAddress "127.0.0.1"
True
-}
isIpAddress :: Ascii -> Bool
isIpAddress a = case strs of
  Just strs' -> helper strs'
  Nothing -> False
  where s = U.toString a
        regex = mkRegex "^([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3})$"
        strs = matchRegex regex s
        helper l = length l == 4 && all helper2 l
        helper2 v = (read v :: Int) >= 0 && (read v :: Int) < 256


{-| ホスト名から、ドメイン名の列を生成する

>>> splitDomains "www.example.com"
[".com",".example.com","www.example.com"]
>>> splitDomains ".example.com"
[".com",".example.com"]
>>> splitDomains "w.example.com"
[".com",".example.com","w.example.com"]
>>> splitDomains "com"
["com"]
>>> splitDomains ""
[]
-}
splitDomains :: Ascii -> [Ascii]
splitDomains host
  | isIpAddress host = [host]
  | otherwise = let f x ys | BS.null x = ys
		           | otherwise = f (BS.dropWhile (/= '.') $ BS.tail x) (x:ys)
                 in f host []

{-|  [from http-conduit] 

>>> pathMatches "/foo" "/foo"
True
>>> pathMatches "/foo" "/bar/baz"
False
>>> pathMatches "/bar/baz" "/"
True
>>> pathMatches "/bar/baz" "/bar"
True
>>> pathMatches "/bar/baz" "/bar/foo"
False
-}
pathMatches :: Ascii -> Ascii -> Bool
pathMatches requestPath cookiePath
  | cookiePath == requestPath = True
  | cookiePath `BS.isPrefixOf` requestPath && BS.singleton (BS.last cookiePath) == "/" = True
  | cookiePath `BS.isPrefixOf` requestPath && BS.singleton (BS.head remainder) == "/" = True
  | otherwise = False
  where remainder = BS.drop (BS.length cookiePath) requestPath



queryCookies :: R.MonadResource m => String -> String -> [SQL.SQLData] -> C.Source m C.Cookie
queryCookies profile sql binds = flip C.PipeM (return ()) $ do
  dbpath <-  (</> pathToProfileDir profile </> "Cookies") <$> liftIO getHomeDirectory
  exists <- liftIO $ doesFileExist dbpath
  when (not exists) $ error $ "No database found:" ++ dbpath
  (dbkey, db) <- R.allocate (SQL.open dbpath) SQL.close
  return $ C.PipeM (return $ stmtSource db sql binds) (R.release dbkey)
      C.$= C.map (\ [ sqlUtc -> cookie_creation_time,
                      sqlText -> cookie_domain,
                      sqlText -> cookie_name,
                      sqlText -> cookie_value,
                      sqlText -> cookie_path,
                      sqlUtc -> cookie_expiry_time,
                      sqlBool -> cookie_secure_only,
                      sqlBool -> cookie_http_only,
                      sqlUtc -> cookie_last_access_time,
                      _,
                      sqlBool -> cookie_persistent ] -> C.Cookie {cookie_host_only=False, ..})
  where
    sqlText (SQL.SQLText x) = BS.pack x
    sqlText _ = error "Column type mismatch."
    sqlUtc (SQL.SQLInteger x) = addUTCTime (fromIntegral $ div x 1000000) (UTCTime (fromGregorian 1601 1 1) (fromInteger 0))
    sqlUtc _ = error "Column type mismatch."
    sqlBool (SQL.SQLInteger x) = if x == 0 then False else True
    sqlBool _ = error "Column type mismatch."





{-
CREATE TABLE cookies (
  creation_utc INTEGER NOT NULL UNIQUE PRIMARY KEY,
  host_key TEXT NOT NULL,
  name TEXT NOT NULL,
  value TEXT NOT NULL,
  path TEXT NOT NULL,
  expires_utc INTEGER NOT NULL,
  secure INTEGER NOT NULL,
  httponly INTEGER NOT NULL,
  last_access_utc INTEGER NOT NULL,
  has_expires INTEGER DEFAULT 1,
  persistent INTEGER DEFAULT 1
  );
-}
loadCookies :: C.MonadResource m => String -> CookieLoader m
loadCookies profile hostname path isSecure isHttpApi =
  queryCookies profile sql pat C.$= C.filter (pathMatches path . C.cookie_path)
  where
    pat = map (SQL.SQLText . BS.unpack) $ splitDomains hostname
    sql = "select * from cookies where "
          ++ (if isSecure then "" else "secure = 0 and ")
	  ++ (if isHttpApi then "" else "httponly = 0 and ")
	  ++ (concat $ intersperse " or " $ replicate (length pat) "host_key=?")
	  ++ " order by expires_utc desc"



{-|
 -}
loadAllCookies :: C.MonadResource m => String -> C.Source m C.Cookie
loadAllCookies profile = queryCookies profile "select * from cookies" []


