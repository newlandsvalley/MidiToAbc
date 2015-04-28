module Test.Samples where

import Euterpea hiding ( Mode(..) )
import Abc.Note
import Abc.Score
import Abc.AbcScore
import Abc.Midi
import Abc.Tuplet
import Codec.Midi

{-
This holds a few convenient functions to try out either full or partial translation of midi into abc

-}


tcd fn = do
          x <- loadMidiTrack 0 fn
          print $ fst3 $ fromMidi x
          print $ snd3 $ fromMidi x
          print $ thd3 $ fromMidi x

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
                 x <- loadMidiTrack 0 fn
                 print $ midiToAbcChars x AbcContext {ctxRhythm = Reel,
                                                     ctxMode = Major,
                                                     ctxKeyName = Dn,
                                                     ctxMode = Major,ctxScale = genScale (Dn, Major),
                                                     ctxLeadIn = (0 / 16),
                                                     ctxTimeSig = (4,4),
                                                     ctxDefaultNoteLen = sn,
                                                     ctxBeats = beats (4,4) }
-}

brudestykke = do
                x <- loadMidiTrack 0  "../midi/brudemarsch1.mid"
                let output = "../abc/brudemarsch1.abc"                  
                writeFile output $ midiToChar x AbcContext {ctxRhythm = Marsch,
                                                 ctxKeyName = An,
                                                 ctxMode = Major,
                                                 ctxScale = genScale (An, Major),
                                                 ctxLeadIn = (0 / 16),
                                                 ctxTimeSig = (2,4),
                                                 ctxDefaultNoteLen = sn,
                                                 ctxBeats = beats (2,4) }
                print $ "file written to " ++ output

hemtilldalen = do
                x <- loadMidiTrack 0  "../midi/hemtilldalen.mid"      
                let output = "../abc/hemtilldalen.abc"                      
                writeFile output $ midiToChar x AbcContext {ctxRhythm = Waltz,
                                                 ctxKeyName = Gn,
                                                 ctxMode = Major,
                                                 ctxScale = genScale (Gn, Major),
                                                 ctxLeadIn = (1 / 4),
                                                 ctxTimeSig = (3,4),
                                                 ctxDefaultNoteLen = en,
                                                 ctxBeats = beats (3,4) }
                print $ "file written to " ++ output


arepolskan = do
                x <- loadMidiTrack 0  "../midi/årepolskan.mid"
                let output = "../abc/årepolskan.abc"                  
                writeFile output $ midiToChar x AbcContext {ctxRhythm = Polska,
                                                 ctxKeyName = Gn,
                                                 ctxMode = Major,
                                                 ctxScale = genScale (Gn, Major),
                                                 ctxLeadIn = (1 / 8),
                                                 ctxTimeSig = (9,8),
                                                 ctxDefaultNoteLen = en,
                                                 ctxBeats = beats (9,8) }
                print $ "file written to " ++ output

antefalk   = do
                x <- loadMidiTrack 0  "../midi/antefalkpolska.mid" 
                let output = "../abc/antefalkpolska.abc"             
                writeFile output $ midiToChar x AbcContext {ctxRhythm = Polska,
                                                 ctxKeyName = Dn,
                                                 ctxMode = Major,
                                                 ctxScale = genScale (Dn, Major),
                                                 ctxLeadIn = (0 / 8),
                                                 ctxTimeSig = (3,4),
                                                 ctxDefaultNoteLen = sn,
                                                 ctxBeats = beats (3,4) }
                print $ "file written to " ++ output

alberta   = do
                x <- loadMidiTrack 0  "../midi/albertaugustssonengelska.mid" 
                let output = "../abc/albertaugustsson.abc"             
                writeFile output $ midiToChar x AbcContext {ctxRhythm = Polska,
                                                 ctxKeyName = An,
                                                 ctxMode = Major,
                                                 ctxScale = genScale (An, Major),
                                                 ctxLeadIn = (1 / 8),
                                                 ctxTimeSig = (2,4),
                                                 ctxDefaultNoteLen = sn,
                                                 ctxBeats = beats (2,4) }
                print $ "file written to " ++ output

amanda = do
                x <- loadMidiTrack 0  "../midi/amanda.mid" 
                let output = "../abc/amanda.abc"             
                print $ midiToAccidentals x AbcContext {ctxRhythm = Polska,
                                                 ctxKeyName = Gn,
                                                 ctxMode = Minor,
                                                 ctxScale = genScale (Gn, Minor),
                                                 ctxLeadIn = (1 / 8),
                                                 ctxTimeSig = (9,8),
                                                 ctxDefaultNoteLen = sn,
                                                 ctxBeats = beats (9,8) }
                -- print $ "file written to " ++ output

harryPotter = do
                x <- loadMidiTrack 1  "../midi/harrypotter.mid"  
                let output = "../abc/harryPotter.abc"             
                writeFile output $  midiToChar x AbcContext {ctxRhythm = Jig,
                                                 ctxKeyName = An,
                                                 ctxMode = Minor,
                                                 ctxScale = genScale (An, Minor),
                                                 ctxLeadIn = (1 / 4),
                                                 ctxTimeSig = (6,8),
                                                 ctxDefaultNoteLen = sn,
                                                 ctxBeats = beats (6,8) }
                print $ "file written to " ++ output
 
abcchord = do
             x <- loadMidiTrack 0  "../midi/abcchord.mid"     
                       
             print $ midiToChar x AbcContext {ctxRhythm = Reel,
                                              ctxKeyName = Gn,
                                              ctxMode = Major,
                                              ctxScale = genScale (Gn, Major),
                                              ctxLeadIn = (0 / 4),
                                              ctxTimeSig = (4,4),
                                              ctxDefaultNoteLen = en,
                                              ctxBeats = beats (4,4) }      
             {-
             print $ fst3 $ fromMidi x      
             -}

testBarline = do
                x <- loadMidiTrack 0  "../midi/brudemarsch1.mid" 
                print $ midiToBarline x AbcContext {ctxRhythm = Marsch,
                                                 ctxKeyName = An,
                                                 ctxMode = Major,
                                                 ctxScale = genScale (An, Major),
                                                 ctxLeadIn = (0 / 16),
                                                 ctxTimeSig = (2,4),
                                                 ctxDefaultNoteLen = sn,
                                                 ctxBeats = beats (2,4)  }

simplifyBrudestykke = do
                x <- loadMidiTrack 0  "../midi/brudemarsch1.mid"
                print $ condense $ removeZeros $ fst3 $ fromMidi x
                
                
testPolska = do
                x <- loadMidiTrack 0  "../midi/årepolskan.mid" 
                print $ midiToBarline x AbcContext {ctxRhythm = Polska,
                                                 ctxKeyName = Gn,
                                                 ctxMode = Major,
                                                 ctxScale = genScale (Gn, Major),
                                                 ctxLeadIn = (1 / 8),
                                                 ctxTimeSig = (9,8),
                                                 ctxDefaultNoteLen = en,
                                                 ctxBeats = beats (9,8) }

testTriplets = do
                x <- loadMidiTrack 0  "../midi/threefourtripletpolska.mid" 
                print $ midiToTuplets x AbcContext {ctxRhythm = Polska,
                                                 ctxKeyName = Dn,
                                                 ctxMode = Major,
                                                 ctxScale = genScale (Dn, Major),
                                                 ctxLeadIn = (0 / 8),
                                                 ctxTimeSig = (9,8),
                                                 ctxDefaultNoteLen = en,
                                                 ctxBeats = beats (9,8) }

testQuadruplets = do
                x <- loadMidiTrack 0  "../midi/quadrupletpolska.mid" 
                print $ midiToBarline x AbcContext {ctxRhythm = Polska,
                                                 ctxKeyName = Dn,
                                                 ctxMode = Major,
                                                 ctxScale = genScale (Dn, Major),
                                                 ctxLeadIn = (0 / 8),
                                                 ctxTimeSig = (9,8),
                                                 ctxDefaultNoteLen = en,
                                                 ctxBeats = beats (9,8) }
                   

simplify fn = do
                 x <- loadMidiTrack 0 fn
                 print $ condense $ removeZeros $ fst3 $ fromMidi x

playIt fn = do
                 x <- loadMidiTrack 0 fn
                 play $ removeZeros $ fst3 $  fromMidi x

