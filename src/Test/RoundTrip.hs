module Test.RoundTrip where


import Euterpea hiding ( Mode(..), KeySig, Phrase )
import Abc.Note
import Abc.Score
import Abc.AbcScore
import Abc.Midi
import Abc.Tuplet
import Abc.Tools
import Codec.Midi

{-
This holds a few samples where a score is produced, converted back to Music and then played
-}


roundTrip ::  Midi -> AbcContext -> Music1
roundTrip m c =  scoreToMusic c $ midiToScore m c


playHtd =  do
                let trackNo = 0
                (m,ts) <- loadMidiTrack trackNo  "../midi/hemtilldalen.mid"      
                let output = "../abc/hemtilldalen.abc"                   
                print $ "time sig " ++ (show ts)   
                let music = roundTrip m AbcContext {ctxTrackNo = trackNo,
                                                 ctxName= "hemtilldalen",
                                                 ctxRhythm = Waltz,
                                                 ctxKeyName = Gn,
                                                 ctxMode = Major,
                                                 ctxScale = genScale (Gn, Major),
                                                 ctxLeadIn = (1 / 4),
                                                 ctxTimeSig = ts,
                                                 ctxDefaultNoteLen = en,
                                                 ctxBeats = beats ts }
                -- print $ "file written to " ++ output
                play music



playAntefalk = do 
                let trackNo = 0
                (m,ts) <- loadMidiTrack trackNo  "../midi/antefalkpolska.mid"          
                print $ "time sig " ++ (show ts)           
                let music = roundTrip m  AbcContext {ctxTrackNo = trackNo,
                                                 ctxName= "antefalkpolska",
                                                 ctxRhythm = Polska,
                                                 ctxKeyName = Dn,
                                                 ctxMode = Major,
                                                 ctxScale = genScale (Dn, Major),
                                                 ctxLeadIn = (0 / 8),
                                                 ctxTimeSig = ts,
                                                 ctxDefaultNoteLen = sn,
                                                 ctxBeats = beats ts }
                play music




playAlberta = do  
                let trackNo = 0
                (m,ts) <- loadMidiTrack trackNo  "../midi/albertaugustssonengelska.mid" 
                       
                print $ "time sig " ++ (show ts) 
                let music = roundTrip m AbcContext {ctxTrackNo = trackNo,
                                                 ctxName= "albertaugustssonengelska",
                                                 ctxRhythm = Polska,
                                                 ctxKeyName = An,
                                                 ctxMode = Major,
                                                 ctxScale = genScale (An, Major),
                                                 ctxLeadIn = (1 / 8),
                                                 ctxTimeSig = ts,
                                                 ctxDefaultNoteLen = sn,
                                                 ctxBeats = beats ts }
                play music

playReceipt = do  
                let trackNo = 1
                (m,ts) <- loadMidiTrack trackNo  "../midi/carolansreceipt.mid"  
                print $ "time sig " ++ (show ts) 

                let music = roundTrip m AbcContext {ctxTrackNo = trackNo,
                                                 ctxName= "Carolans Receipt",
                                                 ctxRhythm = Reel,
                                                 ctxKeyName = Gn,
                                                 ctxMode = Major,
                                                 ctxScale = genScale (Gn, Major),
                                                 ctxLeadIn = (0 / 8),
                                                 ctxTimeSig = ts,
                                                 ctxDefaultNoteLen = sn,
                                                 ctxBeats = beats ts }
                play music







