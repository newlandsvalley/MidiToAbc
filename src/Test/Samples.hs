module Test.Samples where


import Euterpea hiding ( Mode(..), KeySig, Phrase )
import Abc.Note
import Abc.Score
import Abc.AbcScore
import Abc.Midi
import Abc.Tuplet
import Abc.Tools
import Codec.Midi

{-
This holds a few convenient functions to try out either full or partial translation of midi into abc

-}


tcd fn = do
          (x,ts) <- loadMidiTrack 1 fn
          print $ fst3 $ fromMidi x
          print $ snd3 $ fromMidi x
          print $ thd3 $ fromMidi x

midiToNotes :: Midi -> Notes Prim2
midiToNotes m = let m1 = fst3 $ fromMidi m
                           in condense $ removeZeros m1

midiToUnNumberedBarline :: Midi -> AbcContext -> Score Prim2
midiToUnNumberedBarline m c = let
                                m1 = fst3 $ fromMidi m
                              in barline c (condense $ removeZeros m1)

midiToBarline :: Midi -> AbcContext -> Score Prim2
midiToBarline m c = let m1 = fst3 $ fromMidi m
                           in numberBars $ barline c (condense $ removeZeros m1)

midiToTuplets :: Midi -> AbcContext -> Score Prim2
midiToTuplets m c = let m1 = fst3 $ fromMidi m
                           in tuplets c $ numberBars $ barline c (condense $ removeZeros m1)


midiToScore :: Midi -> AbcContext -> Score AbcEntity
midiToScore m c = let m1 = fst3 $ fromMidi m
                           in toAbcScore c $ numberBars $ barline c (condense $ removeZeros m1)

midiToAccidentals :: Midi -> AbcContext -> Score AbcEntity
midiToAccidentals m c = let m1 = fst3 $ fromMidi m
                           in accidentals $ toAbcScore c $ numberBars $ barline c (condense $ removeZeros m1)


midiToFlatScore :: Midi -> AbcContext -> [Char]
midiToFlatScore m c = let m1 = fst3 $ fromMidi m
                           in flattenScore c $ toAbcScore c $ numberBars $ barline c (condense $ removeZeros m1)


{-
abcScore fn = do
                 let trackNo = 0
                 (m,ts) <- loadMidiTrack trackNo fn
                 print $ midiToAbcChars m AbcContext {ctxTrackNo = trackNo,
                                                     ctxName= "abcScore",
                                                     ctxRhythm = Reel,
                                                     ctxMode = Major,
                                                     ctxKeyName = Dn,
                                                     ctxMode = Major,ctxScale = genScale (Dn, Major),
                                                     ctxLeadIn = (0 / 16),
                                                     ctxTimeSig = ts,
                                                     ctxDefaultNoteLen = sn,
                                                     ctxBeats = beats ts }
-}

brudestykke = do 
                let trackNo = 0
                (m,ts) <- loadMidiTrack trackNo  "../midi/brudemarsch1.mid"
                let output = "../abc/brudemarsch1.abc"  
                    cfg = AbcContext {ctxTrackNo = trackNo,
                                      ctxName= "brudemarsch1",
                                      ctxRhythm = Marsch,
                                      ctxKeyName = An,
                                      ctxMode = Major,
                                      ctxScale = genScale (An, Major),
                                      ctxLeadIn = (0 / 16),
                                      ctxTimeSig = ts,
                                      ctxDefaultNoteLen = sn,
                                      ctxBeats = beats ts }   
                         
                print $ "time sig " ++ (show ts) 
                writeFile output $ midiToChar m cfg
                print $ "file written to " ++ output

hemtilldalen = do
                let trackNo = 0
                (m,ts) <- loadMidiTrack trackNo  "../midi/hemtilldalen.mid"      
                let output = "../abc/hemtilldalen.abc"                   
                print $ "time sig " ++ (show ts)   
                writeFile output $ midiToChar m AbcContext {ctxTrackNo = trackNo,
                                                 ctxName= "hemtilldalen",
                                                 ctxRhythm = Waltz,
                                                 ctxKeyName = Gn,
                                                 ctxMode = Major,
                                                 ctxScale = genScale (Gn, Major),
                                                 ctxLeadIn = (1 / 4),
                                                 ctxTimeSig = ts,
                                                 ctxDefaultNoteLen = en,
                                                 ctxBeats = beats ts }
                print $ "file written to " ++ output


arepolskan = do
                let trackNo = 0
                (m,ts) <- loadMidiTrack trackNo  "../midi/årepolskan.mid"
                let output = "../abc/årepolskan.abc"                          
                print $ "time sig " ++ (show ts)           
                writeFile output $ midiToChar m AbcContext {ctxTrackNo = trackNo,
                                                 ctxName= "arepolskan",
                                                 ctxRhythm = Polska,
                                                 ctxKeyName = Gn,
                                                 ctxMode = Major,
                                                 ctxScale = genScale (Gn, Major),
                                                 ctxLeadIn = (1 / 8),
                                                 ctxTimeSig = ts,
                                                 ctxDefaultNoteLen = en,
                                                 ctxBeats = beats ts }
                print $ "file written to " ++ output

antefalk   = do 
                let trackNo = 0
                (m,ts) <- loadMidiTrack trackNo  "../midi/antefalkpolska.mid" 
                let output = "../abc/antefalkpolska.abc"                                   
                print $ "time sig " ++ (show ts)           
                writeFile output $ midiToChar m AbcContext {ctxTrackNo = trackNo,
                                                 ctxName= "antefalkpolska",
                                                 ctxRhythm = Polska,
                                                 ctxKeyName = Dn,
                                                 ctxMode = Major,
                                                 ctxScale = genScale (Dn, Major),
                                                 ctxLeadIn = (0 / 8),
                                                 ctxTimeSig = ts,
                                                 ctxDefaultNoteLen = sn,
                                                 ctxBeats = beats ts }
                print $ "file written to " ++ output




alberta   = do  
                let trackNo = 0
                (m,ts) <- loadMidiTrack trackNo  "../midi/albertaugustssonengelska.mid" 
                let output = "../abc/albertaugustsson.abc"             
                print $ "time sig " ++ (show ts) 
                writeFile output $ midiToChar m AbcContext {ctxTrackNo = trackNo,
                                                 ctxName= "albertaugustssonengelska",
                                                 ctxRhythm = Polska,
                                                 ctxKeyName = An,
                                                 ctxMode = Major,
                                                 ctxScale = genScale (An, Major),
                                                 ctxLeadIn = (1 / 8),
                                                 ctxTimeSig = ts,
                                                 ctxDefaultNoteLen = sn,
                                                 ctxBeats = beats ts }
                print $ "file written to " ++ output


amanda = do
                let trackNo = 0
                (m,ts) <- loadMidiTrack trackNo  "../midi/amanda.mid" 
                let output = "../abc/amanda.abc"                                    
                print $ "time sig " ++ (show ts)                    
                print $ midiToAccidentals m AbcContext {ctxTrackNo = trackNo,
                                                 ctxName= "amanda",
                                                 ctxRhythm = Polska,
                                                 ctxKeyName = Gn,
                                                 ctxMode = Minor,
                                                 ctxScale = genScale (Gn, Minor),
                                                 ctxLeadIn = (1 / 8),
                                                 ctxTimeSig = ts,
                                                 ctxDefaultNoteLen = sn,
                                                 ctxBeats = beats ts }
                -- print $ "file written to " ++ output


pianoSynth = do
                let trackNo = 1
                (m,ts) <- loadMidiTrack trackNo  "../midi/PianoSynth-Track_1-9.mid"  
                let output = "../abc/PianoSynth-Track_1-9.abc"     

                print $ "time sig " ++ (show ts)
                     
                writeFile output $ midiToChar m  AbcContext {ctxTrackNo = trackNo,
                                                 ctxName= "PianoSynth Track 1-9",
                                                 ctxRhythm = Polska,
                                                 ctxKeyName = Dn,
                                                 ctxMode = Major,
                                                 ctxScale = genScale (Dn, Major),
                                                 ctxLeadIn = (0 / 4),
                                                 ctxTimeSig = ts,
                                                 ctxDefaultNoteLen = sn,
                                                 ctxBeats = beats ts }
                
                print $ "file written to " ++ output
 
abcchord = do 
             let trackNo = 0
             (m,ts) <- loadMidiTrack trackNo  "../midi/abcchord.mid"     
                       
             print $ midiToChar m AbcContext {ctxTrackNo = trackNo,
                                              ctxName= "abcchord",
                                              ctxRhythm = Reel,
                                              ctxKeyName = Gn,
                                              ctxMode = Major,
                                              ctxScale = genScale (Gn, Major),
                                              ctxLeadIn = (0 / 4),
                                              ctxTimeSig = ts,
                                              ctxDefaultNoteLen = en,
                                              ctxBeats = beats ts }      
             {-
             print $ fst3 $ fromMidi x      
             -}

testBarline = do  
                let trackNo = 0
                (m,ts) <- loadMidiTrack trackNo  "../midi/brudemarsch1.mid" 
                print $ midiToBarline m AbcContext {ctxTrackNo = trackNo,
                                                 ctxName= "brudemarsch1",
                                                 ctxRhythm = Marsch,
                                                 ctxKeyName = An,
                                                 ctxMode = Major,
                                                 ctxScale = genScale (An, Major),
                                                 ctxLeadIn = (0 / 16),
                                                 ctxTimeSig = ts,
                                                 ctxDefaultNoteLen = sn,
                                                 ctxBeats = beats ts  }

simplifyBrudestykke = do
                let trackNo = 0
                (m,ts) <- loadMidiTrack trackNo  "../midi/brudemarsch1.mid"
                print $ condense $ removeZeros $ fst3 $ fromMidi m
                
                
testPolska = do
                let trackNo = 0
                (m,ts) <- loadMidiTrack trackNo  "../midi/årepolskan.mid" 
                print $ midiToBarline m AbcContext {ctxTrackNo = trackNo,
                                                 ctxName= "arepolskan",
                                                 ctxRhythm = Polska,
                                                 ctxKeyName = Gn,
                                                 ctxMode = Major,
                                                 ctxScale = genScale (Gn, Major),
                                                 ctxLeadIn = (1 / 8),
                                                 ctxTimeSig = ts,
                                                 ctxDefaultNoteLen = en,
                                                 ctxBeats = beats ts }

testTriplets = do     
                let trackNo = 0
                (m,ts) <- loadMidiTrack trackNo  "../midi/threefourtripletpolska.mid" 
                print $ midiToTuplets m AbcContext {ctxTrackNo = trackNo,
                                                 ctxName= "threefourtripletpolska",
                                                 ctxRhythm = Polska,
                                                 ctxKeyName = Dn,
                                                 ctxMode = Major,
                                                 ctxScale = genScale (Dn, Major),
                                                 ctxLeadIn = (0 / 8),
                                                 ctxTimeSig = ts,
                                                 ctxDefaultNoteLen = en,
                                                 ctxBeats = beats ts }

testQuadruplets = do  
                let trackNo = 0
                (m,ts) <- loadMidiTrack trackNo "../midi/quadrupletpolska.mid" 
                print $ midiToBarline m AbcContext {ctxTrackNo = trackNo,
                                                 ctxName= "quadrupletpolska",
                                                 ctxRhythm = Polska,
                                                 ctxKeyName = Dn,
                                                 ctxMode = Major,
                                                 ctxScale = genScale (Dn, Major),
                                                 ctxLeadIn = (0 / 8),
                                                 ctxTimeSig = ts,
                                                 ctxDefaultNoteLen = en,
                                                 ctxBeats = beats ts }

testPianoSynth = do  
                let trackNo = 1
                (m,ts) <- loadMidiTrack trackNo  "../midi/PianoSynth-Track_1-9.mid"  
                let output = "../abc/PianoSynth-Track_1-9.abc"     

                print $ "time sig " ++ (show ts)
                     
                print $ midiToBarline m AbcContext {ctxTrackNo = trackNo,
                                                 ctxName= "PianoSynth Track 1-9",
                                                 ctxRhythm = Polska,
                                                 ctxKeyName = Dn,
                                                 ctxMode = Major,
                                                 ctxScale = genScale (Dn, Major),
                                                 ctxLeadIn = (0 / 4),
                                                 ctxTimeSig = ts,
                                                 ctxDefaultNoteLen = sn,
                                                 ctxBeats = beats ts }
                   

simplify tno fn = do
                 (m,ts) <- loadMidiTrack tno fn
                 print $ condense $ removeZeros $ fst3 $ fromMidi m

playIt tno fn = do
                 (m,ts) <- loadMidiTrack tno fn
                 play $ removeZeros $ fst3 $  fromMidi m





