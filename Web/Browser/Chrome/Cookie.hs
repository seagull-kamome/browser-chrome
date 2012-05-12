{-# LANGUAGE NamedFieldPuns, RecordWildCards, ViewPatterns #-}
{- -}
module Web.Browser.Chrome.Cookie (loadCookies) where

import qualified Data.ByteString.UTF8 as U
import qualified Data.ByteString.Char8 as BS
import Data.List (intersperse)
import Data.Time.Clock (addUTCTime, UTCTime (UTCTime))
import Data.Time.Calendar (fromGregorian)
import qualified Database.SQLite3 as SQL
import Control.Applicative
import Control.Monad (when)
import Control.Exception (bracket)
import qualified Network.HTTP.Conduit as C
import Network.HTTP.Types (Ascii)
import Text.Regex (mkRegex, matchRegex)
import System.Directory (getHomeDirectory,doesFileExist)
import System.FilePath ((</>))

import Web.Browser.Internal

pathToCookieDB :: FilePath -> FilePath
pathToCookieDB profile = ".config" </> "google-chrome" </> profile </> "Cookies"
--pathToCookieDB profile = "Local Settings" </> "Application Data" </> "Google" </> "Chrome" </> "User Data" </> profile </> "Cookies"

{- from http-conduit -}
isIpAddress :: Ascii -> Bool
isIpAddress a = case strs of
  Just strs' -> helper strs'
  Nothing -> False
  where s = U.toString a
        regex = mkRegex "^([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3})$"
        strs = matchRegex regex s
        helper l = length l == 4 && all helper2 l
        helper2 v = (read v :: Int) >= 0 && (read v :: Int) < 256

splitDomains :: Ascii -> [Ascii]
splitDomains host
  | isIpAddress host = [host]
  | otherwise = let f x ys | BS.null x = ys
		           | otherwise = f (BS.dropWhile (/= '.') $ BS.tail x) (x:ys)
                 in f host []

{- from http-conduit -}
pathMatches :: Ascii -> Ascii -> Bool
pathMatches requestPath cookiePath
  | cookiePath == requestPath = True
  | cookiePath `BS.isPrefixOf` requestPath && BS.singleton (BS.last cookiePath) == U.fromString "/" = True
  | cookiePath `BS.isPrefixOf` requestPath && BS.singleton (BS.head remainder) == U.fromString "/" = True
  | otherwise = False
  where remainder = BS.drop (BS.length cookiePath) requestPath



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
  );Local Settings\Application Data\Google\Chrome\User Data\Default
-}
loadCookies :: String -> CookieLoader IO
loadCookies profile hostname path isSecure isHttpApi = do
  dbpath <-  (</> pathToCookieDB profile) <$> getHomeDirectory
  exists <- doesFileExist dbpath
  when (not exists) $ error $ "No database found:" ++ dbpath
  bracket
    (SQL.open dbpath)
    SQL.close
    (\db ->
        bracket
          (SQL.prepare db sql)
	  SQL.finalize
	  $ (\stmt -> SQL.bind stmt (map (SQL.SQLText . BS.unpack) pat) >> SQL.step stmt >>= go stmt []) )
  where
    pat = splitDomains hostname
    sql = "select * from cookies where "
          ++ (if isSecure then "" else "secure = 0 and ")
	  ++ (if isHttpApi then "" else "httponly = 0 and ")
	  ++ (concat $ intersperse " or " $ replicate (length pat) "host_key=?")
	  ++ " order by expires_utc desc"
    go _ xs SQL.Done = return xs
    go stmt xs SQL.Row  =  do
      [ sqlUtc -> cookie_creation_time,
        sqlText -> cookie_domain,
        sqlText -> cookie_name,
        sqlText -> cookie_value,
        sqlText -> cookie_path,
        sqlUtc -> cookie_expiry_time,
        sqlBool -> cookie_secure_only,
        sqlBool -> cookie_http_only,
        sqlUtc -> cookie_last_access_time,
        _,
        sqlBool -> cookie_persistent ] <- SQL.columns stmt
      SQL.step stmt >>= if pathMatches path cookie_path
	                  then go stmt (C.Cookie {cookie_host_only=False, ..}:xs) 
                          else go stmt xs 
    sqlText (SQL.SQLText x) = BS.pack x
    sqlText _ = error "Column type mismatch."
    sqlUtc (SQL.SQLInteger x) = addUTCTime (fromIntegral $ div x 1000000) (UTCTime (fromGregorian 1601 1 1) (fromInteger 0))
    sqlUtc _ = error "Column type mismatch."
    sqlBool (SQL.SQLInteger x) = if x == 0 then False else True
    sqlBool _ = error "Column type mismatch."



