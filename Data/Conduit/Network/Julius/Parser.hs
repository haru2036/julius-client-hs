{-# LANGUAGE OverloadedStrings #-}

module Data.Conduit.Network.Julius.Parser 
(recogText
,recogTextFromDocument
,parseTextFromByteString
,parseTextDefault
,splitMessages
,decodeUtf8
,fromStrictText 
)
where
import Text.XML.Cursor
import Text.XML as X
import Data.Text as T (Text, words, concat, splitOn)
import Data.Text.Encoding as ET (decodeUtf8)
import Data.Text.Lazy.Builder(fromText, toLazyText)
import qualified Data.Text.Internal.Lazy as LT(Text)
import Data.ByteString(ByteString)
import Control.Exception.Base(SomeException)

test :: IO Cursor
test = do
  file <- X.readFile (def :: ParseSettings) "demo.xml"
  return $ fromDocument file

recogText :: Cursor -> [Text]
recogText c = c $// attribute (Name "WORD" Nothing Nothing)

recogTextFromDocument :: Document-> [Text]
recogTextFromDocument = recogText . fromDocument

parseTextFromByteString :: ByteString -> Either SomeException  Document
parseTextFromByteString string = parseText (def :: ParseSettings) $ fromStrictText $ decodeUtf8 string

parseTextDefault :: LT.Text -> Either SomeException Document
parseTextDefault = parseText (def :: ParseSettings)

splitMessages :: Text -> [Text]
splitMessages = splitOn ".\n"

fromStrictText :: Text -> LT.Text
fromStrictText = toLazyText . fromText
