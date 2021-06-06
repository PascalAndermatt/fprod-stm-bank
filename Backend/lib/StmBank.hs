{-# LANGUAGE OverloadedStrings #-}
{-|
    Dieses Modul enthält Funktionen ..
-}
module StmBank
(
    getInitialAccounts, createInitialBank, findBankAccountById,
    BankAccount (..), Bank (..), BankAccountResponse, toBankAccountResponse,
    createAccountFromTriple, BankAccountRequest, createAccountFromRequest,
    addBankAccount, withdraw, deposit, BalanceUpdate, BankAccounts, TransferRequest (..),
    getAccountsForTransfer, StmResult (..), BankException (..), updateAccountBalance,
    getResultOfStmAction, updateAccountStatus, transfer,
    maybeResult, isOwnerInvalid, createIban
)
where

import Control.Monad ( when, unless )
import Control.Concurrent.STM
import Data.Char
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Aeson.Types hiding (Error)
import qualified StmBank.Util as StmUtil
import           Data.Hashable
import           Control.Exception (Exception)



-- BANK DATA TYPES
data Bank                = Bank { accounts:: TVar BankAccounts}
type BankAccounts        = Map String BankAccount
data BankAccount         = BankAccount { iban :: String, name :: String, balance :: TVar Int, active :: TVar Bool }

type BalanceUpdate       = BankAccount -> Int -> STM ()
data StmResult a         = Error String | Result a



-- EXCEPTION TYPES
data BankException = NegativeAmount | AccountOverdrawn |
                     AccountInactive | AccountBalanceNotZero |
                     OwnerLength | OwnerNonAlpha deriving Show
instance Exception BankException



-- REST DTO (Request / Response) Types
data BankAccountResponse = BankAccountResponse { res_iban :: String, res_owner :: String, res_balance :: Int, res_active :: Bool }
data BankAccountRequest  = BankAccountRequest {req_owner:: String, req_balance:: Int }
data TransferRequest     = TransferRequest {from :: String, to :: String, amount :: Int}



-- JSON DATA CONVERSION
instance ToJSON BankAccountResponse where
  toJSON (BankAccountResponse iban owner bal active) = object [
    "iban"      .= (StmUtil.stringToJson iban),
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



-- METHODS
-- create initial bank with list of BankAccounts
createInitialBank :: [BankAccount] -> STM Bank
createInitialBank bankAccounts =  do
                        tVarBankAccounts <- newTVar (Map.fromList (zip keys bankAccounts))
                        pure (Bank tVarBankAccounts)
                        where keys = map iban bankAccounts

-- create initial set of accounts and store in map
getInitialAccounts :: IO [BankAccount]
getInitialAccounts = do
    result <- mapM (\(owner, bal) -> do
        salt <- StmUtil.generateRandomSalt
        pure (owner, bal, salt)) [("Pascal", 100), ("Turan", 200)]
    stmResult <- atomically (mapM (\tri -> getResultOfStmAction (createAccountFromTriple tri)) result)
    maybe (putStrLn "initial BankAccount error" >> pure []) (\l -> pure l) (maybeResult stmResult)

-- create account from triple
createAccountFromTriple :: (String, Int, Int) -> STM (StmResult BankAccount)
createAccountFromTriple (owner, bal, salt) = createAccount owner bal salt

-- create new account
createAccount :: String -> Int -> Int -> STM (StmResult BankAccount)
createAccount owner bal salt = do
  checkCreateAccountArguments owner bal
  let newIban = createIban salt owner
  bankAccount <- (BankAccount newIban owner <$> newTVar bal <*> newTVar True)
  pure (Result bankAccount)

-- store BankAccount in bank
addBankAccount :: TVar BankAccounts -> BankAccount -> STM ()
addBankAccount tVarBankAccounts acc = do
    bankAccounts <- readTVar tVarBankAccounts
    writeTVar tVarBankAccounts (Map.insert (iban acc) acc bankAccounts)

-- return nothing or BankAccount in map
findBankAccountById :: String -> BankAccounts -> Maybe BankAccount
findBankAccountById = Map.lookup

-- create BankAccount from BankAccountRequest
createAccountFromRequest :: BankAccountRequest -> Int -> STM (StmResult BankAccount)
createAccountFromRequest (BankAccountRequest owner bal) randomSalt =  getResultOfStmAction (createAccount owner bal randomSalt)

-- convert BankAccount object to a BankAccountResponse for sending
toBankAccountResponse :: BankAccount -> STM BankAccountResponse
toBankAccountResponse (BankAccount iban owner b a) = do
                      bal <- readTVar b
                      active <- readTVar a
                      pure (BankAccountResponse iban owner bal active)

-- base update operation
updateBalance :: (Int -> Int -> Int) -> BankAccount -> Int -> STM ()
updateBalance updateF acc amount  = do
    when (amount < 0) (throwSTM NegativeAmount)
    isActive <- readTVar (active acc)
    unless isActive (throwSTM AccountInactive)
    bal <- readTVar (balance acc)
    writeTVar (balance acc) (updateF bal amount)

-- perform withdraw operation on account
withdraw :: BankAccount -> Int -> STM ()
withdraw acc amount = do
    bal <- readTVar (balance acc)
    when (amount > bal) (throwSTM AccountOverdrawn)
    updateBalance (-) acc amount

-- perform deposit operation on account
deposit :: BankAccount -> Int -> STM ()
deposit  = updateBalance (+)


-- return nothing or accounts for transfer operation
getAccountsForTransfer :: TransferRequest -> BankAccounts -> Maybe (BankAccount, BankAccount)
getAccountsForTransfer (TransferRequest ibanFrom ibanTo _) bankAccounts = do
        from <- findBankAccountById ibanFrom bankAccounts
        to   <- findBankAccountById ibanTo bankAccounts
        return (from, to)

-- perform transfer operation on two accounts with amount
transfer :: BankAccount -> BankAccount -> Int -> STM (StmResult ())
transfer accountA accountB amount = do
    withdraw accountA amount
    deposit accountB amount
    pure (Result ())


-- update account balance and save in map
updateAccountBalance :: TVar BankAccounts -> BankAccount -> Int -> BalanceUpdate -> STM (StmResult BankAccount)
updateAccountBalance tVarBankAccounts acc amount f = do
  bankAccounts <- readTVar tVarBankAccounts
  f acc amount
  writeTVar tVarBankAccounts (Map.adjust (\_ -> acc) (iban acc) bankAccounts)
  pure (Result acc)

-- update account activity status and save in map
updateAccountStatus :: TVar BankAccounts -> BankAccount -> Bool -> STM (StmResult BankAccount)
updateAccountStatus tVarBankAccounts acc newActive = do
    bankAccounts <- readTVar tVarBankAccounts
    bal <- readTVar (balance acc)
    when (not newActive && bal /= 0) (throwSTM AccountBalanceNotZero)
    writeTVar (active acc) newActive
    writeTVar tVarBankAccounts (Map.adjust (\_ -> acc) (iban acc) bankAccounts)
    pure (Result acc)


-- evaluates list of StmResults for errors
maybeResult :: [StmResult a] -> Maybe [a]
maybeResult [] = Just []
maybeResult ((Result x):xs) = fmap (x:) (maybeResult xs)
maybeResult ((Error _):_) = Nothing

-- return result of STM action or error + message
getResultOfStmAction :: STM (StmResult a) -> STM (StmResult a)
getResultOfStmAction stmA = catchSTM stmA handleException
  where handleException (NegativeAmount)         = pure (Error "Fehler: Der Betrag muss grösser als 0 sein.")
        handleException (AccountOverdrawn)       = pure (Error "Fehler: Das Konto kann nicht überzogen werden.")
        handleException (AccountInactive)        = pure (Error "Fehler: Das Konto ist inaktiv.")
        handleException (AccountBalanceNotZero)  = pure (Error "Fehler: Der Betrag auf dem Konto ist nicht 0.")
        handleException (OwnerLength)            = pure (Error "Fehler: Der Owner muss mind. 3 Zeichen enthalten.")
        handleException (OwnerNonAlpha)          = pure (Error "Fehler: Der Owner darf nur gültige Zeichen enthalten: [a-Z].")



-- HELPER METHODS
-- check given arguments for account creation 
checkCreateAccountArguments :: String -> Int -> STM ()
checkCreateAccountArguments owner bal = do
      when (bal < 0) (throwSTM NegativeAmount)
      when (length owner < 3) (throwSTM OwnerLength)
      when (isOwnerInvalid owner) (throwSTM OwnerNonAlpha)

-- check if owner only contains characters
isOwnerInvalid :: String -> Bool
isOwnerInvalid owner = not (all isLetter owner)


-- create new IBAN number using hashed and salted owner name
createIban :: Int -> String -> String
createIban randomNumber owner = "CH" ++ (show hashVal) ++ (map toUpper (take 3 owner))
                            where hashVal = abs (hashWithSalt randomNumber owner)
