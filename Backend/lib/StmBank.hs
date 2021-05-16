{-|
Dieses Modul enthält Funktionen um...
-}
module StmBank 
(
    
)
where

import Control.Monad ( replicateM_ )
import Control.Concurrent ( threadDelay, forkIO )
import Control.Concurrent.STM
import Data.Char
import qualified Data.Map.Strict as Map

type Map k v = Map.Map k v

data BankAccount = BankAccount { accId :: String, name :: String, balance :: TVar Int }


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


addBankAccount :: TVar (Map Int BankAccount) -> BankAccount -> Int -> STM ()
addBankAccount tVar newAccount accIdNr = do
    accounts <- readTVar tVar
    writeTVar tVar (Map.insert accIdNr newAccount accounts)


createInitialBank :: [BankAccount] -> (Map Int BankAccount)
createInitialBank bankAccounts =  Map.fromList (zip [1..] bankAccounts)


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


