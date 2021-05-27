{-# LANGUAGE OverloadedStrings #-}
{-|
Dieses Modul enthält Funktionen um...
-}
module StmBank 
(
    getInitialAccounts, createInitialBank, findBankAccountById,
    BankAccount (..), Bank (..), BankAccountResponse, toBankAccountResponse, 
    createAccountFromTriple, BankAccountRequest, createAccountFromRequest,
    addBankAccount, withdraw, deposit, BalanceUpdate, BankAccounts, TransferRequest (..),
    maybeAccountsForTransfer, StmResult (..), BankException (..), updateBalanceOfAccountInBank,
    getResultOfStmAction, updateStatusOfAccountInBank, transfer,
    maybeResult, isOwnerInValid, createIban
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

-- Bank Data Types
data Bank                = Bank { accounts:: TVar BankAccounts}
type BankAccounts        = Map String BankAccount
data BankAccount         = BankAccount { ibanNr :: String, name :: String, balance :: TVar Int, active :: TVar Bool }

type BalanceUpdate       = BankAccount -> Int -> STM ()
data StmResult a         = Error String | Result a

-- Exception Types
data BankException = NegativeAmount | AccountOverdrawn | 
                     AccountInactive | AccountBalanceNotZero | 
                     OwnerLength | OwnerNonAlpha deriving Show
instance Exception BankException

-- REST DTO's (Request/Response) Types
data BankAccountResponse = BankAccountResponse { ibanNrR :: String, nameR :: String, balanceR :: Int, activeR :: Bool }
data BankAccountRequest  = BankAccountRequest {nameRequest:: String, balanceRequest:: Int }
data TransferRequest     = TransferRequest {from :: String, to :: String, amount :: Int}

-- JSON converters
instance ToJSON BankAccountResponse where
  toJSON (BankAccountResponse iban owner bal active) = object [
    "ibanNr"    .= (StmUtil.stringToJson iban), 
    "owner"     .= (StmUtil.stringToJson owner), 
    "balance"   .= (toJSON bal),
    "active"    .= (toJSON active)]

instance FromJSON BankAccountRequest where
      parseJSON (Object v) = BankAccountRequest <$>
                             v .:  "owner"    <*>
                             v .:  "balance"

instance FromJSON TransferRequest where
    parseJSON (Object v) = TransferRequest <$>
                            v .: "from" <*>
                            v .: "to"   <*>
                            v .: "amount"

-- Methods
toBankAccountResponse :: BankAccount -> STM BankAccountResponse
toBankAccountResponse (BankAccount i n b a) = do
                      bal <- readTVar b
                      active <- readTVar a
                      pure (BankAccountResponse i n bal active)

maybeAccountsForTransfer :: TransferRequest -> BankAccounts -> Maybe (BankAccount, BankAccount)
maybeAccountsForTransfer (TransferRequest ibanFrom ibanTo _) bankAccounts = do
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
    stmResult <- atomically (mapM (\tri -> getResultOfStmAction (createAccountFromTriple tri)) result)
    maybe (putStrLn "initial bankAccount error" >> pure []) (\l -> pure l) (maybeResult stmResult)

-- printT :: [(String, Int, Int)] -> IO ()
-- printT [] = putStrLn "empty triple list" >> pure()
-- printT (x:xs) = putStrLn (show x) >> printT xs

-- printt :: [StmResult BankAccount] -> IO ()
-- printt [] = putStrLn "empty list" >> pure ()
-- printt ((Result bankAcc):xs) = showAccount bankAcc >>= putStrLn >> (printt xs)
-- printt ((Error msg):xs) = putStrLn msg >> (printt xs)

maybeResult :: [StmResult a] -> Maybe [a]
maybeResult [] = Just []
maybeResult ((Result x):xs) = fmap (x:) (maybeResult xs)
maybeResult ((Error _):_) = Nothing

createAccountFromTriple :: (String, Int, Int) -> STM (StmResult BankAccount)
createAccountFromTriple (owner, initBal, randomSalt) = createAccount owner initBal randomSalt

createAccountFromRequest :: BankAccountRequest -> Int -> STM (StmResult BankAccount)
createAccountFromRequest (BankAccountRequest owner bal) randomSalt =  getResultOfStmAction (createAccount owner bal randomSalt)

-- can throw an Exception
createAccount :: String -> Int -> Int -> STM (StmResult BankAccount)
createAccount owner initBal randomSalt = do
  throwStmExceptionWhenDataIsInvalid owner initBal
  let iban = createIban randomSalt owner 
  bankAccount <- (BankAccount iban owner <$> newTVar initBal <*> newTVar True)
  pure (Result bankAccount)


throwStmExceptionWhenDataIsInvalid :: String -> Int -> STM ()
throwStmExceptionWhenDataIsInvalid owner bal = do
      when (bal < 0) (throwSTM NegativeAmount)
      when (length owner < 3) (throwSTM OwnerLength)
      when (isOwnerInValid owner) (throwSTM OwnerNonAlpha)

isOwnerInValid :: String -> Bool
isOwnerInValid owner = not (all isLetter owner)

-- can throw an Exception
withdraw :: BankAccount -> Int -> STM ()
withdraw bankAcc amount = do
    bal <- readTVar (balance bankAcc)
    when (amount > bal) (throwSTM AccountOverdrawn)
    updateBalance (-) bankAcc amount

-- can throw an Exception
deposit :: BankAccount -> Int -> STM ()
deposit  = updateBalance (+)
    

updateBalance :: (Int -> Int -> Int) -> BankAccount -> Int -> STM ()
updateBalance updateF bankAcc amount  = do
    when (amount < 0) (throwSTM NegativeAmount)
    isActive <- readTVar (active bankAcc)
    when (not isActive) (throwSTM AccountInactive)
    bal <- readTVar (balance bankAcc)
    writeTVar (balance bankAcc) (updateF bal amount)

transfer :: BankAccount -> BankAccount -> Int -> STM (StmResult ())
transfer accountA accountB amount = do
    withdraw accountA amount
    deposit accountB amount
    pure (Result ())

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

updateStatusOfAccountInBank :: TVar BankAccounts -> BankAccount -> Bool -> STM (StmResult BankAccount)
updateStatusOfAccountInBank tVarBankAccounts acc newActive = do
    bankAccounts <- readTVar tVarBankAccounts
    bal <- readTVar (balance acc)
    when (not newActive && bal /= 0) (throwSTM AccountBalanceNotZero)
    writeTVar (active acc) newActive
    writeTVar tVarBankAccounts (Map.adjust (\_ -> acc) (ibanNr acc) bankAccounts)
    pure (Result acc)


-- catchSTM :: Exception e => STM a -> (e -> STM a) -> STM a
getResultOfStmAction :: STM (StmResult a) -> STM (StmResult a)
getResultOfStmAction stmA = catchSTM stmA handleException
  where handleException (NegativeAmount)         = pure (Error "Fehler: Der Betrag muss grösser als 0 sein.")
        handleException (AccountOverdrawn)       = pure (Error "Fehler: Das Konto kann nicht überzogen werden")
        handleException (AccountInactive)        = pure (Error "Fehler: Das Konto ist inaktiv")
        handleException (AccountBalanceNotZero)  = pure (Error "Fehler: Der Betrag auf dem Konto ist nicht 0")
        handleException (OwnerLength)            = pure (Error "Fehler: Der Owner muss mind. 3 Zeichen enthalten")
        handleException (OwnerNonAlpha)          = pure (Error "Fehler: Der Owner darf nur gültige Zeichen enthalten: [a-Z]")

showAccount :: BankAccount -> IO String
showAccount (BankAccount iban owner tVarbal tVarActive) = do
    bal <- readTVarIO tVarbal
    isActive <- readTVarIO tVarActive
    pure ("Bankaccount: id: " ++ iban ++ ", name: " ++ owner ++ ", balance: " ++ show bal ++ ", active: " ++ show isActive)

showAllAccounts :: [BankAccount] -> IO ()
showAllAccounts accs = do
    accountsAsText <- mapM showAccount accs
    mapM_ putStrLn accountsAsText