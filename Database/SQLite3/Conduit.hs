{-|

-}
module Database.SQLite3.Conduit where

import Control.Applicative
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Resource
import Control.Monad.Reader
import qualified Data.Conduit as C
import Database.SQLite3
import System.Directory (doesFileExist)

type SQLite3 m = ReaderT Database m

withSQLite3 :: (MonadResource m) => FilePath -> (Database -> m a) -> m a
withSQLite3 dbpath m = do
  exists <- liftIO $ doesFileExist dbpath
  when (not exists) $ error $ "No database found:" ++ dbpath
  (dbkey, db) <- allocate (open dbpath) close
  ret <- m db
  release dbkey
  return ret


runSQLite3 :: MonadResource m => FilePath -> SQLite3 m a -> m a
runSQLite3 dbpath reader = withSQLite3 dbpath (runReaderT reader)


stmtSource :: MonadResource m => Database -> String -> [SQLData] -> C.Source m [SQLData]
stmtSource db sql bindvar = flip C.PipeM (return ()) $ do
  (stmtkey, stmt) <- allocate (prepare db sql) finalize
  liftIO $ bind stmt bindvar
  let fin = release stmtkey
  let go = flip C.PipeM fin $ do
                                res <- liftIO $ step stmt
                                case res of
                                  Done -> return (C.Done Nothing ())
                                  Row -> C.HaveOutput go fin <$> liftIO (columns stmt)
  return go


