{-# LANGUAGE OverloadedStrings #-}
{-|
Dieses Modul enthält Funktionen um...
-}
module StmBank 
(
    getInitialAccounts, createInitialBank, findBankAccountById,
    BankAccount, Bank (..), BankAccountResponse, toBankAccountResponse, 
    createAccountFromTuple, accounts, ibanNr, BankAccountRequest, createAccountFromRequest,
    addBankAccount, withDraw
)
where

import Control.Monad ( replicateM_ )
import Control.Concurrent ( threadDelay, forkIO )
import Control.Concurrent.STM
import Data.Char
import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy as T
import           Data.Aeson.Types
import qualified StmBank.Util as StmUtil
import           System.Random (randomRIO)
import           Data.Hashable

type Map k v = Map.Map k v

data BankAccount         = BankAccount { ibanNr :: String, name :: String, balance :: TVar Int }
data BankAccountResponse = BankAccountResponse { ibanNrR :: String, nameR :: String, balanceR :: Int }
data Bank                = Bank { accounts:: Map String BankAccount}
data BankAccountRequest  = BankAccountRequest {nameRequest:: String, balanceRequest:: Int }


instance ToJSON BankAccountResponse where
  toJSON (BankAccountResponse iban owner bal) = object [
    "ibanNr" .= (StmUtil.stringToJson iban), 
    "owner" .= (StmUtil.stringToJson owner), 
    "balance" .= (toJSON bal)]


toBankAccountResponse :: BankAccount -> IO BankAccountResponse
toBankAccountResponse (BankAccount i n b) = do
                      bal <- readTVarIO b
                      pure (BankAccountResponse i n bal)

instance FromJSON BankAccountRequest where
      parseJSON (Object v) = BankAccountRequest <$>
                             v .:  "owner"    <*>
                             v .:  "balance"

findBankAccountById :: String -> Bank -> Maybe BankAccount
findBankAccountById ibanNr bank = Map.lookup ibanNr (accounts bank)

getInitialAccounts :: IO [BankAccount]
getInitialAccounts = mapM createAccountFromTuple [("Pascal", 100), ("Turan", 200)]

createAccountFromTuple :: (String, Int) -> IO BankAccount
createAccountFromTuple (name, initBal) = createAccount name initBal

createAccountFromRequest :: BankAccountRequest -> IO BankAccount
createAccountFromRequest (BankAccountRequest owner bal) = createAccount owner bal

createAccount :: String -> Int -> IO BankAccount
createAccount owner initBal = do
  iban <- getIban owner
  atomically ((BankAccount iban owner) <$> newTVar initBal)

withDraw :: BankAccount -> Int -> STM ()
withDraw (BankAccount _ _ tVarBal) amount = do
    bal <- readTVar tVarBal
    writeTVar tVarBal (bal - amount)

deposit :: BankAccount -> Int -> STM ()
deposit bankAcc amount = do
    bal <- readTVar (balance bankAcc)
    writeTVar (balance bankAcc) (bal + amount)

transfer :: BankAccount -> BankAccount -> Int -> STM ()
transfer accountA accountB amount = do
    withDraw accountA amount
    deposit accountB amount


addBankAccount :: TVar Bank -> BankAccount -> STM ()
addBankAccount tVarBank newAccount = do
    bank <- readTVar tVarBank
    writeTVar tVarBank (Bank (Map.insert (ibanNr newAccount) newAccount (accounts bank)))


createInitialBank :: [BankAccount] -> IO (TVar Bank)
createInitialBank bankAccounts =  newTVarIO (Bank (Map.fromList (zip keys bankAccounts)))
          where keys = map ibanNr bankAccounts


getIban :: String -> IO String
getIban owner = do
  randomNum <- randomRIO (0,100)
  let hashVal = abs (hashWithSalt randomNum owner)
  pure ("CH" ++ (show hashVal)  ++ (map toUpper (take 3 owner)))


showAccount :: BankAccount -> IO String
showAccount (BankAccount iban owner tVarbal) = do
    bal <- readTVarIO tVarbal
    pure ("Bankaccount: id: " ++ iban ++ ", name: " ++ owner ++ ", balance: " ++ show bal)


showAllAccounts :: [BankAccount] -> IO ()
showAllAccounts accs = do
    accountsAsText <- mapM showAccount accs
    mapM_ putStrLn accountsAsText


