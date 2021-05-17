{-# LANGUAGE OverloadedStrings #-}
{-|
Dieses Modul enthält Funktionen um...
-}
module StmBank.Util 
(
    stringToJson, generateRandomSalt
)
where

import qualified Data.Text.Lazy as T
import           Data.Aeson.Types
import           System.Random (randomRIO)

stringToJson :: String -> Value
stringToJson s = toJSON (T.pack s)

generateRandomSalt :: IO Int
generateRandomSalt = randomRIO (0,100)