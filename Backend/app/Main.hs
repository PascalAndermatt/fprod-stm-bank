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
import           Control.Exception (Exception)

-- |Haupteinstiegspunkt, startet den Webserver.
main :: IO ()
main = do
  accs <- StmBank.getInitialAccounts -- create initial account list
  bank <- atomically (StmBank.createInitialBank accs) -- create bank with accounts
  
  scotty 4000 $ do
    middleware logStdoutDev

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
      newAccount <- runStmActionAtomically (StmBank.createAccountFromRequest bankAccountRequest randomSalt)
      runStmActionAtomically (StmBank.addBankAccount (StmBank.accounts bank) newAccount)
      status status201

    post "/accounts/:id/withdraw" $ do
      iban <- param "id"
      amount <- param "amount"
      -- adjust :: Ord k => (a -> a) -> k -> Map k a -> Map k a
      -- modifyTVar :: TVar a -> (a -> a) -> STM ()
      let tVarBankAccounts = (StmBank.accounts bank)
      bankAccounts <- liftIO (readTVarIO tVarBankAccounts)
      let maybeAccount = StmBank.findBankAccountById iban bankAccounts

      withAccount (\acc -> do
        -- runStmActionAtomically (updateBalanceOfAccountInBank tVarBankAccounts acc amount StmBank.deposit)
          result <- runStmActionAtomically (getResultOfStmAction (updateBalanceOfAccountInBank tVarBankAccounts acc amount StmBank.withdraw))
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
        -- runStmActionAtomically (updateBalanceOfAccountInBank tVarBankAccounts acc amount StmBank.deposit)
          result <- runStmActionAtomically (getResultOfStmAction (updateBalanceOfAccountInBank tVarBankAccounts acc amount StmBank.deposit))
          createResponse result (\res -> do
            response <- runStmActionAtomically (StmBank.toBankAccountResponse res)
            json (toJSON response))
        ) maybeAccount
    
    post "/accounts/transfer" $ do
      transferRequest <- jsonData :: ActionM StmBank.TransferRequest

      let tVarBankAccounts = (StmBank.accounts bank)
      bankAccounts <- liftIO (readTVarIO tVarBankAccounts)

      runStmActionAtomically (StmBank.transferFromRequest transferRequest bankAccounts)

    get "/test/error" $ do
      status status500
      json (StmUtil.stringToJson "Dies ist eine Test Fehlermeldung")


withAccount :: (StmBank.BankAccount -> ActionM()) -> Maybe StmBank.BankAccount -> ActionM ()
withAccount = maybe (status status404)

updateBalanceOfAccountInBank :: TVar StmBank.BankAccounts -> StmBank.BankAccount -> Int -> StmBank.BalanceUpdate -> STM (StmBank.StmResult StmBank.BankAccount)
updateBalanceOfAccountInBank tVarBankAccounts acc amount f = do
  bankAccounts <- readTVar tVarBankAccounts
  f acc amount
  writeTVar tVarBankAccounts (Map.adjust (\_ -> acc) (StmBank.ibanNr acc) bankAccounts)
  pure (StmBank.Result acc)


-- catchSTM :: Exception e => STM a -> (e -> STM a) -> STM a
getResultOfStmAction :: STM (StmBank.StmResult a) -> STM (StmBank.StmResult a)
getResultOfStmAction stmA = catchSTM stmA handleException
  where handleException (StmBank.NegativeAmount)   = pure (StmBank.Error "Fehler: Der Betrag muss grösser als 0 sein.")
        handleException (StmBank.AccountOverdrawn) = pure (StmBank.Error "Fehler: Das Konto kann nicht überzogen werden")


createResponse :: StmBank.StmResult a -> (a -> ActionM ()) -> ActionM ()
createResponse (StmBank.Error message) _ = (do
                                status status412
                                json (StmUtil.stringToJson message))
createResponse (StmBank.Result r) f = f r


runStmActionAtomically :: STM a -> ActionM a
runStmActionAtomically stmAction = liftIO (atomically stmAction)