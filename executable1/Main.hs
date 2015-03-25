{-# LANGUAGE OverloadedStrings #-}
import Data.Conduit.Network.Julius.Parser
import Data.Conduit.Network
import Data.Conduit
import Debug.Trace
import qualified Data.Conduit.List as CL
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Maybe

main :: IO()
main = runGeneralTCPClient (clientSettings 10500 "127.0.0.1") runner

runner :: AppData -> IO()
runner appData = (appSource appData) $$ decodeConduit =$= parseConduit =$ (CL.mapM_ print)

parseConduit :: Monad m => Conduit T.Text m [T.Text] 
parseConduit = do
  ma <- await
  case ma of
    Just a -> do 
      let messages = trace (T.unpack a) (splitMessages a)
      --let messages = splitMessages a
      yield (Prelude.concat $ Prelude.map (listWords . parseTextDefault . fromStrictText) messages) >> parseConduit  
    Nothing -> return ()

listWords (Right parsed) = recogTextFromDocument parsed
listWords (Left _) = []

decodeConduit :: Monad m => Conduit BS.ByteString m T.Text
decodeConduit = do
  ma <- CL.head
  case ma of
    Just a -> do 
      yield (decodeUtf8 a) >> decodeConduit
    Nothing -> return ()

test :: IO [T.Text]
test = do
  str <- Prelude.readFile "demo.xml"
  let txt = T.pack str
  let messages = Prelude.init $ splitMessages $ txt
  print messages
  print $ parseMessage $ head messages
  print $ Prelude.concat $ Prelude.map parseMessage messages
  return messages

parseMessage :: T.Text -> [T.Text]
parseMessage = listWords . parseTextDefault . fromStrictText
