module Abc.Metadata ( headers ) where

import Abc.Note ( Rhythm, Mode, KeyName, KeySig, TimeSig, AbcContext (..) )
import Euterpea.Music.Note.Music ( Dur )
import Data.Char
import Data.Ratio
import Control.Monad.Reader


-- description of supported abc headers
headers :: Reader AbcContext String
headers = do        
            rhythm  <- asks ctxRhythm
            keyName <- asks ctxKeyName
            mode    <- asks ctxMode
            timeSig <- asks ctxTimeSig
            noteLen <- asks ctxDefaultNoteLen
            return $ tuneHeaders rhythm keyName mode timeSig noteLen

tuneHeaders :: Rhythm -> KeyName -> Mode -> TimeSig -> Dur -> String
tuneHeaders r k m t l = unlines [
                    xHeader
                  , tHeader "TitleHere"
                  , rHeader $ show r
                  , kHeader k m
                  , mHeader t
                  , lHeader l ]

kHeader :: KeyName -> Mode -> String 
kHeader kn m = let mode = take 3 $ map toLower (show m)
                in  "K: " ++ show kn ++ mode

xHeader :: String 
xHeader = "X: 1"

tHeader :: String -> String
tHeader t = "T: " ++ t 

rHeader :: String -> String
rHeader r = "R: " ++ r

mHeader :: TimeSig -> String 
mHeader (n,d) = "M: " ++ show n ++ "/" ++ show d

lHeader :: Dur -> String
lHeader l = "L: " ++ show (numerator l) ++ "/" ++ show (denominator l) 



