{-# LANGUAGE OverloadedStrings #-}

module Data.Conduit.Network.Julius.Parser where
import Text.XML.Cursor
import Text.XML as X
import Data.Text(Text)

test :: IO Cursor
test = do
  file <- X.readFile (def :: ParseSettings) "demo.xml"
  return $ fromDocument file
{-

whypos :: Cursor -> [Whypo]
whypos = map cWhypo findWhypos

cWhypo :: Cursor -> Whypo
cWhypo = node

cursorElement :: Cursor -> Element
cursorElement =

findWhypos :: Cursor -> [Cursor]
findWhypos c = c $// element "WHYPO"
-}

recogText :: Cursor -> [Text]
recogText c = c $// attribute (Name "WORD" Nothing Nothing)
