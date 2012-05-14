module Web.Browser.Chrome.LocalStorage where

import Control.Applicative ((<$>))
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Resource (MonadResource)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as C
import Data.Maybe (catMaybes)
import qualified Data.ByteString.Char8 as B8
import qualified Database.SQLite3 as SQL
import Network.HTTP.Types (Ascii)
import System.Directory (getDirectoryContents)
import System.FilePath ((</>))
import Text.Regex(mkRegex, matchRegex)

import Web.Browser.Internal
import Web.Browser.Chrome.Utils (pathToLocalStorageDir)
import Database.SQLite3.Conduit


data LocalStorageDesc = LocalStorageDesc { filename, protocol, domain :: String, magicNumber :: Int }

enumLocalStorages ::
  String  -- User Profile
  -> C.Source IO LocalStorageDesc
enumLocalStorages profile = flip C.PipeM (return ()) $ do
  xs <- pathToLocalStorageDir profile >>= getDirectoryContents
  return $ C.sourceList $ catMaybes $ map f xs
  where
    f ys = matchRegex (mkRegex "^(http|https)_(.+)_([0-9])*\\.localstorage$") ys
             >>= (\[proto, domain, magic] -> return $ LocalStorageDesc ys proto domain $ read magic)




type LocalStorage m = SQLite3 m

runLocalStorage :: (MonadResource m) => LocalStorage m a -> LocalStorageDesc -> String -> m a
runLocalStorage st dsc profile = do
  dbpath <- liftIO $ (</> filename dsc) <$> pathToLocalStorageDir profile
  runSQLite3 dbpath st


get :: MonadResource m => String -> LocalStorage m (Maybe B8.ByteString)
get k = do
  db <- ask
  stmtSource db "select value from ItemTable where key=?" [SQL.SQLText k] C.$$ C.await >>= sqlBS
  where sqlBS (Just [(SQL.SQLBlob bs)]) = return $ Just bs
        sqlBS (Just _) = error "Column type mismatch."
	sqlBS Nothing = return Nothing

