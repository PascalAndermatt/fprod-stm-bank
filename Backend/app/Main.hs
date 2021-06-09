{-# LANGUAGE OverloadedStrings #-}

{-|
    This module contains the main entrypoint and logic for the server 
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


-- MAIN ENTRYPOINT, STARTS SERVER 
main :: IO ()
main = do
  accs <- StmBank.getInitialAccounts -- create initial account list
  bank <- atomically (StmBank.createInitialBank accs) -- create bank with accounts
  let tVarBankAccounts = StmBank.accounts bank
  
  -- start scotty server
  scotty 4000 $ do
    middleware logStdoutDev
    middleware CorsUtil.corsified

    -- main route
    get "/" $ file "static/index.html"

    -- accounts route
    get "/accounts" $ do
      bankAccounts <- liftIO (readTVarIO tVarBankAccounts)
      response <- runStmActionAtomically (mapM StmBank.toBankAccountResponse (Map.elems bankAccounts))
      json (toJSON response)

    get "/accounts/:id" $ do
      iban <- param "id"

      withAccount (\acc -> do
        response <- runStmActionAtomically (StmBank.toBankAccountResponse acc)
        json (toJSON response)) iban tVarBankAccounts

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
      handleBalanceUpdateRequest tVarBankAccounts StmBank.withdraw

    post "/accounts/:id/deposit" $ do
      handleBalanceUpdateRequest tVarBankAccounts StmBank.deposit
    
    post "/accounts/transfer" $ do
      transferRequest <- jsonData :: ActionM StmBank.TransferRequest
      let amount = StmBank.amount transferRequest

      bankAccounts <- liftIO (readTVarIO tVarBankAccounts)

      maybe (status status412 >> json (StmUtil.stringToJson "Fehler: mind. 1 Konto nicht gefunden.")) (\(from,to) -> do
          result <- runStmActionAtomically (StmBank.getResultOfStmAction (StmBank.transfer from to amount))
          createResponse result (\_ -> status status200)
        ) (StmBank.getAccountsForTransfer transferRequest bankAccounts)

    post "/accounts/close/:id" $ do
      iban <- param "id"

      withAccount (\acc -> do
          result <- runStmActionAtomically ( StmBank.getResultOfStmAction (StmBank.updateAccountStatus tVarBankAccounts acc False))
          createResponse result (\res -> do
            response <- runStmActionAtomically (StmBank.toBankAccountResponse res)
            json (toJSON response))
        ) iban tVarBankAccounts



-- HELPER METHODS
-- perform STM Action atomically
runStmActionAtomically :: STM a -> ActionM a
runStmActionAtomically stmAction = liftIO (atomically stmAction)

-- perform handler function with account, return error if no account found or result otherwise
withAccount :: (StmBank.BankAccount -> ActionM()) -> String -> TVar StmBank.BankAccounts -> ActionM ()
withAccount handleResult iban tVarBankAccounts = do
              bankAccounts <- liftIO (readTVarIO tVarBankAccounts)
              let maybeAccount = StmBank.findBankAccountById iban bankAccounts
              maybe (status status404 >> json (StmUtil.stringToJson "Fehler: Konto wurde nicht gefunden.")) handleResult maybeAccount

-- create response
createResponse :: StmBank.StmResult a -> (a -> ActionM ()) -> ActionM ()
createResponse (StmBank.Error message) _ = (do
                                status status412
                                json (StmUtil.stringToJson message))
createResponse (StmBank.Result r) f = f r

-- perform balance update
handleBalanceUpdateRequest :: TVar StmBank.BankAccounts -> StmBank.BalanceUpdate -> ActionM ()
handleBalanceUpdateRequest tVarBankAccounts balanceUpdateAction = do
      iban <- param "id"
      amount <- param "amount"
      
      withAccount (\acc -> do
          result <- runStmActionAtomically (StmBank.getResultOfStmAction (StmBank.updateAccountBalance tVarBankAccounts acc amount balanceUpdateAction))
          createResponse result (\res -> do
            response <- runStmActionAtomically (StmBank.toBankAccountResponse res)
            json (toJSON response))
        ) iban tVarBankAccounts