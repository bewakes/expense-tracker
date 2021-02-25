module App where

import           Network.Wai.Handler.Warp    as Warp

import           API
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Logger        (runStderrLoggingT)
import           Database.Persist
import           Database.Persist.Postgresql
import           Models
import           Network.Wai
import           Servant                     (Server, serve)
import           Servant.API


server :: ConnectionPool -> Server Api
server pool = expenseListH :<|> expensePostH :<|> userListH :<|> userAddH
    where expenseListH = liftIO expenseGet
          expenseGet :: IO [Expense]
          expenseGet = flip runSqlPersistMPool pool $ do
              expenses <- selectList [] []
              return $ entityVal <$> expenses

          expensePostH exp = liftIO $ expenseAdd exp
          expenseAdd :: Expense -> IO (Maybe (Key Expense))
          --       exists <- selectFirst [UserName ==. (userName newUser)] []
          expenseAdd exp = flip runSqlPersistMPool pool $ Just <$> insert exp

          dbpool = flip runSqlPersistMPool pool

          userListH = liftIO userGet
          userGet :: IO [User]
          userGet = dbpool $ do
              users <- selectList [] []
              return $ entityVal <$> users

          userAddH usr = liftIO $ userAdd usr
          userAdd :: User -> IO (Maybe (Key User))
          userAdd usr = dbpool $ Just <$> insert usr

app :: ConnectionPool -> Application
app pool = serve api $ server pool

mkApp :: ConnectionString -> IO Application
mkApp cstr = do
    pool <- runStderrLoggingT $ createPostgresqlPool cstr 10
    flip runSqlPersistMPool pool $ do
        runMigration migrateAll
    return $ app pool

run :: ConnectionString -> Int -> IO ()
run cstr port = do
    app <- mkApp cstr
    Warp.run port app
