{-|

-}
module Database.SQLite3.Conduit where

import Control.Applicative
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Resource
import Control.Monad.Reader
import qualified Data.Conduit as C
import Database.SQLite3

type SQLite3 = ReaderT Database (ResourceT IO)

runSQLite3 :: Database -> SQLite3 a -> ResourceT IO a
runSQLite3 db reader = runReaderT reader db


stmtSource :: MonadResource m => Database -> String -> [SQLData] -> C.Source m [SQLData]
stmtSource db sql bindvar = flip C.PipeM (return ()) $ do
  (stmtkey, stmt) <- allocate (prepare db sql) finalize
  liftIO $ bind stmt bindvar
  let fin = release stmtkey
  let go = flip C.PipeM fin $ do
                                res <- liftIO $ step stmt
                                case res of
                                  Done -> pure $ C.Done Nothing ()
                                  Row -> C.HaveOutput go fin <$> liftIO (columns stmt)
  return go


