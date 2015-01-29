module Data.Conduit.Network.Julius.Parser where
import Data.Conduit.Network.Julius.Types
import Text.XML.Light

parse :: Element -> Either String JuliusMessage
parse xElem = case (qName $ elName xElem) of
                "STARTPROC" -> Right StartProc
                "ENDPROC" -> Right EndProc
                "STARTRECOG" -> Right StartRecog
                _ -> Left "UnExpected Element"
