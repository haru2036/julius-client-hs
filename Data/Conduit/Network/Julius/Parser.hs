{-# LANGUAGE OverloadedStrings #-}

module Data.Conduit.Network.Julius.Parser where
import Text.XML.Cursor
import Text.XML as X
import Data.Text(Text)
import Data.Text.Lazy.Encoding(decodeUtf8)
import Data.ByteString.Lazy.Internal(ByteString)
import Control.Exception.Base(SomeException)

test :: IO Cursor
test = do
  file <- X.readFile (def :: ParseSettings) "demo.xml"
  return $ fromDocument file
{-
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

recogTextFromDocument :: Document-> [Text]
recogTextFromDocument = recogText . fromDocument

parseTextFromByteString :: ByteString -> Either SomeException  Document
parseTextFromByteString string = parseText (def :: ParseSettings) $ decodeUtf8 string


