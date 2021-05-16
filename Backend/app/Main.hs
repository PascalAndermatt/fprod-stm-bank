{-# LANGUAGE OverloadedStrings #-}

{-|
Dieses Modul enthält den Einstiegspunkt um die Applikation zu starten.
-}
module Main where

import qualified StmBank
import qualified Data.Text.Lazy as T
import           Control.Monad.IO.Class (liftIO)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           System.Directory (listDirectory)
import           Web.Scotty (file, get, html, middleware, param, post, scotty)
import           Data.Hashable

-- |Haupteinstiegspunkt, startet den Webserver.
main :: IO ()
main = scotty 4000 $ do
  middleware logStdoutDev

  get "/" $ file "static/index.html"

  get "/accounts/:id" $ do
    accountId <- param "id" -- id as String
    -- liftIO (putStrLn ("account with id: " ++ accountId))
    file "static/index.html"

  post "/name" $ do
    -- name <- param "Name" -- Parameter aus dem Form
    -- IO Actions müssen mit liftIO zu einer ActionM 'angehoben' werden
    file "static/index.html"

  get "/list/:foldername" $ do
    file "static/index.html"
