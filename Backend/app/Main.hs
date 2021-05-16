{-# LANGUAGE OverloadedStrings #-}

{-|
Dieses Modul enthält den Einstiegspunkt um die Applikation zu starten.
-}
module Main where

import qualified StmBank
import qualified Data.Text.Lazy as T
import           Control.Monad.IO.Class (liftIO)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           System.Directory (listDirectory)
import           Web.Scotty
import           Data.Hashable
import           Data.Aeson.Types
import Control.Concurrent.STM
import           Network.HTTP.Types

-- |Haupteinstiegspunkt, startet den Webserver.
main :: IO ()
main = do
  accs <- atomically StmBank.getInitialAccounts
  bank <- pure (StmBank.createInitialBank accs)
  putStrLn "test"
  
  scotty 4000 $ do
    middleware logStdoutDev

    get "/" $ file "static/index.html"

    get "/accounts/:id" $ do
      accountId <- param "id" -- id as String
      let maybeAccount = StmBank.findBankAccountById accountId bank
      maybe (status status404) (\acc -> do
        response <- liftIO (StmBank.toBankAccountResponse acc)
        status status200
        json (toJSON response)) maybeAccount

            
      -- acc <- liftIO (atomically (StmBank.createAccountFromTriple ("123", "Peter", 100)))
      -- response <- liftIO (StmBank.toBankAccountResponse acc)

    
      -- liftIO (putStrLn ("account with id: " ++ accountId))
      -- status status200 -- set status code to 200
      -- json (toJSON response) -- send responsebody as json

    post "/name" $ do
      -- name <- param "Name" -- Parameter aus dem Form
      -- IO Actions müssen mit liftIO zu einer ActionM 'angehoben' werden
      file "static/index.html"

    get "/list/:foldername" $ do
      file "static/index.html"





stringToJson :: String -> Value
stringToJson s = toJSON (T.pack s)




