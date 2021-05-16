{-# LANGUAGE OverloadedStrings #-}
{-|
Dieses Modul enthält Funktionen um...
-}
module StmBank.Util 
(
    stringToJson
)
where

import qualified Data.Text.Lazy as T
import           Data.Aeson.Types

stringToJson :: String -> Value
stringToJson s = toJSON (T.pack s)