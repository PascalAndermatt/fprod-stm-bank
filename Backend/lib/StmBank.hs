{-# LANGUAGE OverloadedStrings #-}
{-|
Dieses Modul enthält Funktionen um...
-}
module StmBank 
(
    getInitialAccounts, createInitialBank, findBankAccountById,
    BankAccount, Bank, BankAccountResponse, toBankAccountResponse, 
    createAccountFromTuple, accounts
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


instance ToJSON BankAccountResponse where
  toJSON (BankAccountResponse iban owner bal) = object [
    "ibanNr" .= (StmUtil.stringToJson iban), 
    "owner" .= (StmUtil.stringToJson owner), 
    "balance" .= (toJSON bal)]


toBankAccountResponse :: BankAccount -> IO BankAccountResponse
toBankAccountResponse (BankAccount i n b) = do
                      bal <- readTVarIO b
                      pure (BankAccountResponse i n bal)

-- instance FromJSON Person where
--      parseJSON (Object v) = Person <$>
--                             v .:  "firstName"    <*>
--                             v .:  "lastName"

findBankAccountById :: String -> Bank -> Maybe BankAccount
findBankAccountById ibanNr bank = Map.lookup ibanNr (accounts bank)

getInitialAccounts :: IO [BankAccount]
getInitialAccounts = mapM createAccountFromTuple [("Pascal", 100), ("Turan", 200)]

createAccountFromTuple :: (String, Int) -> IO BankAccount
createAccountFromTuple (name, initBal) = createAccount name initBal

createAccount :: String -> Int -> IO BankAccount
createAccount name initBal = do
  iban <- getIban name
  atomically ((BankAccount iban name) <$> newTVar initBal)

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


createInitialBank :: [BankAccount] -> Bank
createInitialBank bankAccounts =  Bank (Map.fromList (zip keys bankAccounts))
          where keys = map ibanNr bankAccounts


-- getIban2 :: String -> String -> String
-- getIban2 nr owner = "CH" ++ nr ++ "00" ++ (map toUpper (take 3 owner))

getIban :: String -> IO String
getIban owner = do
  randomNum <- randomRIO (0,100)
  let hashVal = abs (hashWithSalt randomNum owner)
  pure ("CH" ++ (show hashVal)  ++ (map toUpper (take 3 owner)))

-- createInitialBankAccounts :: [BankAccount]
-- createInitialBankAccounts = [BankAccount "42" "Pascal" 1000, BankAccount "43" "Turan" 2000]

showAccount :: BankAccount -> IO String
showAccount (BankAccount iban owner bal) = do
    balance <- readTVarIO bal
    pure ("Bankaccount: id: " ++ iban ++ ", name: " ++ owner ++ ", balance: " ++ show balance)


showAllAccounts :: [BankAccount] -> IO ()
showAllAccounts accs = do
    accountsAsText <- mapM showAccount accs
    mapM_ putStrLn accountsAsText


