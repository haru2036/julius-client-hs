module Data.Conduit.Network.Julius.Types  where

import Data.Text(Text) 

data JuliusMessage = StartProc 
                       | EndProc 
                       | StartRecog 
                       | EndRecog 
                       | IStatus {inputStatus :: InputStatus, time :: Int} 
                       | InputParam {frames :: Int, msec :: Int} 
                       | GMM {gmmResult :: Text, cmScore :: Double}
                       | RecogOut {recogOut :: RecognitionOut}
                       | RecogFail
                       | Rejected {reason :: Text}

data InputStatus = Listen | StartRec | EndRec

type RecognitionOut = [Shypo]

data Shypo = Shypo {rank :: Int
                   ,score :: Double
                   ,gram :: Int
                   ,whypos :: [Whypo]
                   }

data Whypo = Whypo {word :: Text
                   ,classId :: Int
                   ,phone :: Text
                   ,cm :: Double
                   }
sentenceWords :: Shypo -> [Text]
sentenceWords = whyposWords . whypos

whyposWords :: [Whypo] -> [Text]
whyposWords = map word
