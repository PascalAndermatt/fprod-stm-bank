{-# LANGUAGE OverloadedStrings #-}
{-|
Dieses Modul enthält Funktionen um...
-}
module StmBank 
(
    getInitialAccounts, createInitialBank, findBankAccountById
    , BankAccount, Bank, BankAccountResponse, toBankAccountResponse, createAccountFromTriple
)
where

import Control.Monad ( replicateM_ )
import Control.Concurrent ( threadDelay, forkIO )
import Control.Concurrent.STM
import Data.Char
import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy as T
import           Data.Aeson.Types

type Map k v = Map.Map k v

data BankAccount = BankAccount { accId :: String, name :: String, balance :: TVar Int }
data BankAccountResponse = BankAccountResponse { accIdr :: String, namer :: String, balancer :: Int }
data Bank = Bank { accounts:: Map Int BankAccount}


instance ToJSON BankAccountResponse where
  toJSON (BankAccountResponse ibanNr owner bal) = object ["ibanNr" .= (stringToJson ibanNr), "owner" .= (stringToJson owner), "balance" .= (toJSON bal)]


toBankAccountResponse :: BankAccount -> IO BankAccountResponse
toBankAccountResponse (BankAccount i n b) = do
                      bal <- readTVarIO b
                      pure (BankAccountResponse i n bal)

stringToJson :: String -> Value
stringToJson s = toJSON (T.pack s)

-- instance FromJSON Person where
--      parseJSON (Object v) = Person <$>
--                             v .:  "firstName"    <*>
--                             v .:  "lastName"

findBankAccountById :: Int -> Bank -> Maybe BankAccount
findBankAccountById accountId bank = Map.lookup accountId (accounts bank)

getInitialAccounts :: STM [BankAccount]
getInitialAccounts = mapM createAccountFromTriple [("1","Pascal", 100), ("2","Turan", 200)]

createAccountFromTriple :: (String, String, Int) -> STM BankAccount
createAccountFromTriple (id, name, initBal) = createAccount id name initBal

createAccount :: String -> String -> Int -> STM BankAccount
createAccount accountId name initBal = (BankAccount (getIban accountId name) name) <$> newTVar initBal

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


addBankAccount :: TVar Bank -> BankAccount -> Int -> STM ()
addBankAccount tVarBank newAccount accIdNr = do
    bank <- readTVar tVarBank
    writeTVar tVarBank (Bank (Map.insert accIdNr newAccount (accounts bank)))


createInitialBank :: [BankAccount] -> Bank
createInitialBank bankAccounts =  Bank (Map.fromList (zip [1..] bankAccounts))


getIban :: String -> String -> String
getIban nr owner = "CH" ++ nr ++ "00" ++ (map toUpper (take 3 owner))

-- createInitialBankAccounts :: [BankAccount]
-- createInitialBankAccounts = [BankAccount "42" "Pascal" 1000, BankAccount "43" "Turan" 2000]

showAccount :: BankAccount -> IO String
showAccount (BankAccount accId name bal) = do
    balance <- readTVarIO bal
    pure ("Bankaccount: id: " ++ accId ++ ", name: " ++ name ++ ", balance: " ++ show balance)


showAllAccounts :: [BankAccount] -> IO ()
showAllAccounts accs = do
    accountsAsText <- mapM showAccount accs
    mapM_ putStrLn accountsAsText


