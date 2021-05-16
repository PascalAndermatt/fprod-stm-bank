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
import           Control.Concurrent.STM
import           Network.HTTP.Types
import qualified StmBank.Util as StmUtil
import qualified Data.Map.Strict as Map

-- |Haupteinstiegspunkt, startet den Webserver.
main :: IO ()
main = do
  accs <- StmBank.getInitialAccounts -- create initial account list
  tVarBank <- liftIO (StmBank.createInitialBank accs) -- create bank with accounts
  
  scotty 4000 $ do
    middleware logStdoutDev

    get "/" $ file "static/index.html"

    get "/accounts" $ do
      bank <- liftIO (readTVarIO tVarBank)
      response <- liftIO (mapM StmBank.toBankAccountResponse (Map.elems (StmBank.accounts bank)))
      json (toJSON response)

    get "/accounts/:id" $ do
      accountId <- param "id" -- id as String
      bank <- liftIO (readTVarIO tVarBank)
      let maybeAccount = StmBank.findBankAccountById accountId bank
      maybe (status status404) (\acc -> do
        response <- liftIO (StmBank.toBankAccountResponse acc)
        status status200
        json (toJSON response)) maybeAccount

    post "/accounts" $ do
      bankAccountRequest <- jsonData :: ActionM StmBank.BankAccountRequest
      newAccount <- liftIO (StmBank.createAccountFromRequest bankAccountRequest)
      liftIO (atomically (StmBank.addBankAccount tVarBank newAccount))
      status status201

    post "/name" $ do
      -- name <- param "Name" -- Parameter aus dem Form
      file "static/index.html"