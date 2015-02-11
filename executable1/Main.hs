{-# LANGUAGE OverloadedStrings #-}
import Data.Conduit.Network.Julius.Parser
import Data.Conduit.Network
import Data.Conduit
import Data.Conduit.List as CL
import Data.ByteString
import Data.Text(Text)
import Data.Maybe

main :: IO()
main = runTCPClient (clientSettings 10500 "127.0.0.1") runner

runner :: AppData -> IO()
runner appData = (appSource appData) $$ decodeConduit =$= parseConduit =$ (CL.mapM_ print)

parseConduit :: Monad m => Conduit Text m [Text] 
parseConduit = do
  a <- await
  let messages = splitMessages $ fromJust a
  yield $ Prelude.concat $ Prelude.map (listWords . parseTextDefault . fromStrictText) messages

listWords (Right parsed) =  recogTextFromDocument parsed
listWords (Left err) = error $ "Parse Error : " ++ show err 

decodeConduit :: Monad m => Conduit ByteString m Text
decodeConduit = do
  a <- await
  yield $ decodeUtf8 $ fromJust a
