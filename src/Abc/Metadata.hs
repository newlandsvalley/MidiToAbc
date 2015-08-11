module Abc.Metadata ( headers ) where

import Abc.Note ( Rhythm, Mode, KeyName, KeySig, TimeSig, BPM, AbcContext (..), displayRhythm )
import Euterpea.Music.Note.Music ( Dur )
import Data.Char
import Data.Ratio
import Control.Monad.Reader


-- description of supported abc headers
headers :: Reader AbcContext String
headers = do        
            name    <- asks ctxName
            rhythm  <- asks ctxRhythm
            keyName <- asks ctxKeyName
            mode    <- asks ctxMode
            timeSig <- asks ctxTimeSig
            bpm     <- asks ctxBPM
            noteLen <- asks ctxDefaultNoteLen
            return $ tuneHeaders name rhythm keyName mode timeSig bpm noteLen

tuneHeaders :: String -> Rhythm -> KeyName -> Mode -> TimeSig -> BPM -> Dur -> String
tuneHeaders n r k m t b l = unlines [
                    xHeader
                  , tHeader n
                  , rHeader $ displayRhythm r
                  , kHeader k m
                  , mHeader t
                  , lHeader l
                  , qHeader b
                  , nHeader ]

kHeader :: KeyName -> Mode -> String 
kHeader kn m = let mode = take 3 $ map toLower (show m)
                in  "K: " ++ show kn ++ mode

xHeader :: String 
xHeader = "X: 1"

nHeader :: String 
nHeader = "N: Generated by miditoabc (https://github.com/newlandsvalley/MidiToAbc)"

tHeader :: String -> String
tHeader t = "T: " ++ t 

rHeader :: String -> String
rHeader r = "R: " ++ r

mHeader :: TimeSig -> String 
mHeader (n,d) = "M: " ++ show n ++ "/" ++ show d

lHeader :: Dur -> String
lHeader l = "L: " ++ show (numerator l) ++ "/" ++ show (denominator l) 

qHeader :: BPM -> String 
qHeader b = "Q: 1/4=" ++ show b



