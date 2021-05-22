{-# LANGUAGE OverloadedStrings #-}
{-|
Dieses Modul enthält Funktionen um...
-}
module StmBank 
(
    getInitialAccounts, createInitialBank, findBankAccountById,
    BankAccount, Bank (..), BankAccountResponse, toBankAccountResponse, 
    createAccountFromTriple, ibanNr, BankAccountRequest, createAccountFromRequest,
    addBankAccount, withdraw, deposit, BalanceUpdate, BankAccounts, TransferRequest,
    transferFromRequest, StmResult (..), BankException (..), updateBalanceOfAccountInBank,
    getResultOfStmAction
)
where

import Control.Monad ( replicateM_, when )
import Control.Concurrent ( threadDelay, forkIO )
import Control.Concurrent.STM
import Data.Char
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy as T
import           Data.Aeson.Types hiding (Error)
import qualified StmBank.Util as StmUtil
import           Data.Hashable
import           Control.Exception (Exception)

-- type Map k v = Map.Map k v

data BankAccount         = BankAccount { ibanNr :: String, name :: String, balance :: TVar Int }
data BankAccountResponse = BankAccountResponse { ibanNrR :: String, nameR :: String, balanceR :: Int }
type BankAccounts        = Map String BankAccount
data Bank                = Bank { accounts:: TVar BankAccounts}
data BankAccountRequest  = BankAccountRequest {nameRequest:: String, balanceRequest:: Int }
data TransferRequest     = TransferRequest {from :: String, to :: String, amount :: Int}
type BalanceUpdate       = BankAccount -> Int -> STM ()
data StmResult a         = Error String | Result a

data BankException = NegativeAmount | AccountOverdrawn deriving Show
instance Exception BankException


instance ToJSON BankAccountResponse where
  toJSON (BankAccountResponse iban owner bal) = object [
    "ibanNr" .= (StmUtil.stringToJson iban), 
    "owner" .= (StmUtil.stringToJson owner), 
    "balance" .= (toJSON bal)]


toBankAccountResponse :: BankAccount -> STM BankAccountResponse
toBankAccountResponse (BankAccount i n b) = do
                      bal <- readTVar b
                      pure (BankAccountResponse i n bal)

instance FromJSON BankAccountRequest where
      parseJSON (Object v) = BankAccountRequest <$>
                             v .:  "owner"    <*>
                             v .:  "balance"

instance FromJSON TransferRequest where
    parseJSON (Object v) = TransferRequest <$>
                            v .: "from" <*>
                            v .: "to"   <*>
                            v .: "amount"


transferFromRequest :: TransferRequest -> BankAccounts -> STM ()
transferFromRequest (TransferRequest from to amount) bankAccounts = do
            maybe (pure ()) (\(f,t) -> transfer f t amount) (maybeAccountsForTransfer from to bankAccounts)


maybeAccountsForTransfer :: String -> String -> BankAccounts -> Maybe (BankAccount, BankAccount)
maybeAccountsForTransfer ibanFrom ibanTo bankAccounts = do
        from <- findBankAccountById ibanFrom bankAccounts
        to   <- findBankAccountById ibanTo bankAccounts
        return (from,to)

findBankAccountById :: String -> BankAccounts -> Maybe BankAccount
findBankAccountById = Map.lookup

getInitialAccounts :: IO [BankAccount]
getInitialAccounts = do
    result <- mapM (\(o,b) -> do
        randomSalt <- StmUtil.generateRandomSalt
        pure (o,b,randomSalt)) [("Pascal", 100), ("Turan", 200)]
    atomically (mapM createAccountFromTriple result)

createAccountFromTriple :: (String, Int, Int) -> STM BankAccount
createAccountFromTriple (owner, initBal, randomSalt) = createAccount owner initBal randomSalt

createAccountFromRequest :: BankAccountRequest -> Int -> STM BankAccount
createAccountFromRequest (BankAccountRequest owner bal) = createAccount owner bal

createAccount :: String -> Int -> Int -> STM BankAccount
createAccount owner initBal randomSalt = do
  let iban = createIban randomSalt owner 
  BankAccount iban owner <$> newTVar initBal

withdraw :: BankAccount -> Int -> STM ()
withdraw (BankAccount _ _ tVarBal) amount = do
        when (amount < 0) (throwSTM NegativeAmount)
        bal <- readTVar tVarBal
        when (amount > bal) (throwSTM AccountOverdrawn)
        writeTVar tVarBal (bal - amount)

        
deposit :: BankAccount -> Int -> STM ()
deposit bankAcc amount = do
    bal <- readTVar (balance bankAcc)
    writeTVar (balance bankAcc) (bal + amount)

transfer :: BankAccount -> BankAccount -> Int -> STM ()
transfer accountA accountB amount = do
    withdraw accountA amount
    deposit accountB amount


addBankAccount :: TVar BankAccounts -> BankAccount -> STM ()
addBankAccount tVarBankAccounts newAccount = do
    bankAccounts <- readTVar tVarBankAccounts
    writeTVar tVarBankAccounts (Map.insert (ibanNr newAccount) newAccount bankAccounts)


createInitialBank :: [BankAccount] -> STM Bank
createInitialBank bankAccounts =  do
                        tVarBankAccounts <- newTVar (Map.fromList (zip keys bankAccounts))
                        pure (Bank tVarBankAccounts)
                        where keys = map ibanNr bankAccounts


createIban :: Int -> String -> String
createIban randomNumber owner = "CH" ++ (show hashVal) ++ (map toUpper (take 3 owner))
                            where hashVal = abs (hashWithSalt randomNumber owner)


updateBalanceOfAccountInBank :: TVar BankAccounts -> BankAccount -> Int -> BalanceUpdate -> STM (StmResult BankAccount)
updateBalanceOfAccountInBank tVarBankAccounts acc amount f = do
  bankAccounts <- readTVar tVarBankAccounts
  f acc amount
  writeTVar tVarBankAccounts (Map.adjust (\_ -> acc) (ibanNr acc) bankAccounts)
  pure (Result acc)


-- catchSTM :: Exception e => STM a -> (e -> STM a) -> STM a
getResultOfStmAction :: STM (StmResult a) -> STM (StmResult a)
getResultOfStmAction stmA = catchSTM stmA handleException
  where handleException (NegativeAmount)   = pure (Error "Fehler: Der Betrag muss grösser als 0 sein.")
        handleException (AccountOverdrawn) = pure (Error "Fehler: Das Konto kann nicht überzogen werden")


showAccount :: BankAccount -> IO String -- evtl. STM Action
showAccount (BankAccount iban owner tVarbal) = do
    bal <- readTVarIO tVarbal
    pure ("Bankaccount: id: " ++ iban ++ ", name: " ++ owner ++ ", balance: " ++ show bal)


showAllAccounts :: [BankAccount] -> IO ()
showAllAccounts accs = do
    accountsAsText <- mapM showAccount accs
    mapM_ putStrLn accountsAsText


