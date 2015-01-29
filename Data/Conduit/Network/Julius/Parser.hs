{-# LANGUAGE OverloadedStrings #-}

module Data.Conduit.Network.Julius.Parser where
import Data.Conduit.Network.Julius.Types
import Text.XML.Cursor
import qualified Data.Text as T (Text)

parse :: Element -> Either String JuliusMessage
parse xElem = case (qName $ elName xElem) of
                "STARTPROC" -> Right StartProc
                "ENDPROC" -> Right EndProc
                "STARTRECOG" -> Right StartRecog
                "ENDRECOG" -> Right EndRecog
                "RecogOut" -> Right $ RecogOut $ parseRecogOut xElem
                _ -> Left "UnExpected Element"

parseRecogOut :: Element -> recogOut
parseRecogOut xElem = map parseShypo $ elContent xElem

parseShypo xElem = Shypo rnk scr grm wps
            where attribs = elAttribs xElem
                  rnk = find attribs


