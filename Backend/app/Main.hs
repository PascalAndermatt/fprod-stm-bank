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
import qualified StmBank.Util as StmUtil
import qualified StmBank.Cors as CorsUtil


-- |Haupteinstiegspunkt, startet den Webserver.
main :: IO ()
main = do
  accs <- StmBank.getInitialAccounts -- create initial account list
  bank <- atomically (StmBank.createInitialBank accs) -- create bank with accounts
  
  scotty 4000 $ do
    middleware logStdoutDev
    middleware CorsUtil.corsified

    get "/" $ file "static/index.html"


    get "/accounts" $ do
      bankAccounts <- liftIO (readTVarIO (StmBank.accounts bank))
      response <- runStmActionAtomically (mapM StmBank.toBankAccountResponse (Map.elems bankAccounts))
      json (toJSON response)

    get "/accounts/:id" $ do
      accountId <- param "id"
      bankAccounts <- liftIO (readTVarIO (StmBank.accounts bank))
      let maybeAccount = StmBank.findBankAccountById accountId bankAccounts
      withAccount (\acc -> do
        response <- runStmActionAtomically (StmBank.toBankAccountResponse acc)
        json (toJSON response)) maybeAccount

    post "/accounts" $ do
      bankAccountRequest <- jsonData :: ActionM StmBank.BankAccountRequest
      randomSalt <- liftIO StmUtil.generateRandomSalt
      stmResult <- runStmActionAtomically (StmBank.createAccountFromRequest bankAccountRequest randomSalt)
      createResponse stmResult (\acc -> do
              runStmActionAtomically (StmBank.addBankAccount (StmBank.accounts bank) acc)
              response <- runStmActionAtomically (StmBank.toBankAccountResponse acc)
              json (toJSON response)
              status status201
        )
      
    post "/accounts/:id/withdraw" $ do
      iban <- param "id"
      amount <- param "amount"

      let tVarBankAccounts = (StmBank.accounts bank)
      bankAccounts <- liftIO (readTVarIO tVarBankAccounts)
      let maybeAccount = StmBank.findBankAccountById iban bankAccounts

      withAccount (\acc -> do
          result <- runStmActionAtomically (StmBank.getResultOfStmAction (StmBank.updateBalanceOfAccountInBank tVarBankAccounts acc amount StmBank.withdraw))
          createResponse result (\res -> do
            response <- runStmActionAtomically (StmBank.toBankAccountResponse res)
            json (toJSON response))
        ) maybeAccount

    post "/accounts/:id/deposit" $ do
      iban <- param "id"
      amount <- param "amount"

      let tVarBankAccounts = (StmBank.accounts bank)
      bankAccounts <- liftIO (readTVarIO tVarBankAccounts)

      let maybeAccount = StmBank.findBankAccountById iban bankAccounts

      withAccount (\acc -> do
          result <- runStmActionAtomically (StmBank.getResultOfStmAction (StmBank.updateBalanceOfAccountInBank tVarBankAccounts acc amount StmBank.deposit))
          createResponse result (\res -> do
            response <- runStmActionAtomically (StmBank.toBankAccountResponse res)
            json (toJSON response))
        ) maybeAccount
    
    post "/accounts/transfer" $ do
      transferRequest <- jsonData :: ActionM StmBank.TransferRequest
      let amount = StmBank.amount transferRequest

      bankAccounts <- liftIO (readTVarIO (StmBank.accounts bank))

      maybe (status status412 >> json (StmUtil.stringToJson "Fehler: mind. 1 Konto nicht gefunden")) (\(from,to) -> do
          result <- runStmActionAtomically (StmBank.getResultOfStmAction (StmBank.transfer from to amount))
          createResponse result (\_ -> status status200)
        ) (StmBank.maybeAccountsForTransfer transferRequest bankAccounts)

    post "/accounts/close/:id" $ do
      iban <- param "id"

      let tVarBankAccounts = (StmBank.accounts bank)
      bankAccounts <- liftIO (readTVarIO tVarBankAccounts)
      let maybeAccount = StmBank.findBankAccountById iban bankAccounts

      withAccount (\acc -> do
          result <- runStmActionAtomically ( StmBank.getResultOfStmAction (StmBank.updateStatusOfAccountInBank tVarBankAccounts acc False))
          createResponse result (\res -> do
            response <- runStmActionAtomically (StmBank.toBankAccountResponse res)
            json (toJSON response))
        ) maybeAccount

withAccount :: (StmBank.BankAccount -> ActionM()) -> Maybe StmBank.BankAccount -> ActionM ()
withAccount = maybe (status status404 >> json (StmUtil.stringToJson "Konto wurde nicht gefunden"))

createResponse :: StmBank.StmResult a -> (a -> ActionM ()) -> ActionM ()
createResponse (StmBank.Error message) _ = (do
                                status status412
                                json (StmUtil.stringToJson message))
createResponse (StmBank.Result r) f = f r

runStmActionAtomically :: STM a -> ActionM a
runStmActionAtomically stmAction = liftIO (atomically stmAction)