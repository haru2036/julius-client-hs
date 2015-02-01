{-# LANGUAGE OverloadedStrings #-}
import Data.Conduit.Network.Julius.Parser
import Data.Conduit.Network
import Data.Conduit
import Data.Conduit.List as CL
import Data.ByteString
import Data.ByteString.Lazy (fromStrict)
import Data.Text(Text)
import Data.Maybe

main :: IO()
main = do
  runTCPClient (clientSettings 10500 "localhost") runner

runner :: AppData -> IO()
runner appData = (appSource appData) $$ parseConduit =$ (CL.mapM_ print)

parseConduit :: Monad m => Conduit ByteString m [Text] 
parseConduit = do
  a <- await
  let eitherParsed = parseTextFromByteString $ fromStrict $ fromJust a
  let texts = case eitherParsed of
        Right parsed -> recogTextFromDocument parsed
        _ -> error "Parse Error"
  yield $ texts
