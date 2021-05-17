{-# LANGUAGE OverloadedStrings #-}

{-|
Dieses Modul enthält den Einstiegspunkt um die Applikation zu starten.
-}
module Main where

import qualified StmBank
import           Control.Monad.IO.Class (liftIO)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Web.Scotty
import           Data.Aeson.Types
import           Control.Concurrent.STM
import           Network.HTTP.Types
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

    post "/accounts/:id/withdraw" $ do
      iban <- param "id"
      amount <- param "amount"
      liftIO (putStrLn "fdg")
      -- adjust :: Ord k => (a -> a) -> k -> Map k a -> Map k a
      -- modifyTVar :: TVar a -> (a -> a) -> STM ()
      bank <- liftIO (readTVarIO tVarBank)
      let maybeAccount = StmBank.findBankAccountById iban bank

      maybe (status status404) (\acc -> do
        liftIO (atomically (modifyBank tVarBank acc amount))
        response <- liftIO (StmBank.toBankAccountResponse acc)
        status status200
        json (toJSON response)) maybeAccount



modifyBank :: TVar StmBank.Bank -> StmBank.BankAccount -> String -> STM ()
modifyBank tVarBank account amount = do
  bank <- readTVar tVarBank
  StmBank.withDraw account (read amount :: Int)
  writeTVar tVarBank (StmBank.Bank (Map.adjust (\_ -> account) (StmBank.ibanNr account) (StmBank.accounts bank)))