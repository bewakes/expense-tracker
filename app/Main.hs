{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Logger        (runStderrLoggingT)
import           Database.Persist
import           Database.Persist.Postgresql
import           Network.Wai

import           App
import           Models

connStr = "host=localhost dbname=expense user=postgres password=postgres port=5433"

port = 8888

main :: IO ()
main = do
    print $ "server listening on " ++ show port ++ " ..."
    run connStr port
