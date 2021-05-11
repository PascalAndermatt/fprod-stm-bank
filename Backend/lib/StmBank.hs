{-|
Dieses Modul enthält Funktionen um HTML zu generieren.
-}
module StmBank where

-- |Erzeugt ein HTML Dokument mit dem gegebenen String als h1 Text.
createPage :: String -> String
createPage title = unlines' [
  "<!doctype html>",
  "<html lang='en'>",
  "  <head></head>",
  "  <body>",
  "    <h1>" ++ title ++ "</h1>",
  "  </body>",
  "</html>"]

-- |Nur hier um zu testen :)
unlines' :: [String] -> String
unlines' [] = ""
unlines' [s] = s
unlines' (s:s':ss) = s  ++ "\n" ++ unlines' (s':ss)
