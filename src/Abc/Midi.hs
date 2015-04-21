module Abc.Midi where

import Euterpea
import Abc.Note
import Abc.Score
import Abc.AbcScore
import Abc.Metadata
import Abc.Tuplet
import Codec.Midi
import Control.Monad.Reader

type IsBarLine = Bool
type IsTie = Bool

-- Work out if a bar line and/or a tie needs to be generated for the next note
-- the incoming parameters are the context and the durations of the note and its offset in a tune
-- the result is a 5-tuple indicating if a bar line and tie is needed, and splitting the note if the latter
-- and also detecting if the note occurs on a beat of the bar
detectBarline :: AbcContext -> Dur -> Dur -> (IsBarLine, IsTie, IsOnBeat, Dur, Dur)
detectBarline c noteDur tuneDur  = 
            let timeSig = (ctxTimeSig c) 
                leadInDur = (ctxLeadIn c)
                beats = (ctxBeats c)
                note = toMeasure noteDur
                offset = toMeasure tuneDur
                -- bar = toMeasure $ tsToDur timeSig
                bar = measuresPerBar timeSig
                leadIn = toMeasure leadInDur
                spaceRemaining = bar - ((offset - leadIn) `mod` bar)
                onBeat = spaceRemaining `elem` beats
             in 
                if (spaceRemaining == note) then
                  (True, False, onBeat, noteDur, (0 /1))
                else if (note <= spaceRemaining) then
                  (False, False, onBeat, noteDur, (0 /1))
                else 
                  (True, True, onBeat, (toDur spaceRemaining), (toDur $note - spaceRemaining))

-- create a score of Prim2 notes taking account of bars, notes tied across bars and beats in the bar.
-- this inserts Bar nodes into the tree
-- Bars are unnumbered at this stage
barline :: AbcContext -> Notes Prim2 -> Score Prim2
barline c n = nFold fn s mul n where
     fn nt@(Note2 d ofs p onBeat) =  Bar 0 (PrimNote nt) EndScore -- all we can do is return Bar for a primitive although these are not yet fully formed
     
     fn (Rest2 d) = error "barline: unexpected Rest" 
     
     mul d ms = error "barline: unexpected multiplet" 
     
     s (Bar _ n1 next1) (Bar _ n2 next2) =  
             let (PrimNote (Note2 d ofs p _)) = n1         -- n1 is the next incoming note to be attached to the rest of the tree
                 (isBarLine, isTie, isOnBeat, noteD, overspillD) = detectBarline c d ofs
                 newn1 = PrimNote (Note2 d ofs p isOnBeat) -- newn1 is identical to n1 but with the beat now detected
                            in if (isBarLine) then
                                  if (isTie) then
                                       -- split the note over the bar - with the tie 
                                       let lhNote = PrimNote (Note2 noteD ofs p isOnBeat)
                                           rhNote = PrimNote (Note2 overspillD ofs p False)
                                           tie = PrimNote TiedNote
                                         in Bar 0 lhNote (Bar 0 (tie :+++: rhNote) next2)
                                   else
                                       Bar 0 newn1 (Bar 0 n2  next2)
                                 else
                                       Bar 0 (newn1 :+++: n2)  next2                 
         
      
     
       -- s x y = error "barline: unexpected node in Notes Prim2 tree"


-- It seems that fromMidi always produces a Music tree of a regular shape.  This has (Rest, Note) pairs all
-- tied together with the (:=:) parallel constructor.  In other words, each of these Rest,Note phrases is
-- played in parallel, but the rest is of a length identical to that of the tune played thus far.  This
-- function converts a Music1 tree into a Notes tree of (intermediary) Prim2  values, where the incoming volume
-- attribute is ignored but each intermediate tune duration (taken from the rest) is added to the note.  
-- The parallel constructor is replaced with the serial (:+:) constructor so that we now have a sequential 
-- composition of notes, each of which knows its offset into the overall tune.
--
-- The function also normalises time durations to exact multiples of the unitDur
--
-- whether a note lies on a regular beat is not registered at this stage 
--
condense :: Music1 -> Notes Prim2
condense = mFold fn s p modify where
    fn (Note d (p,v))   = PrimNote (Note2 (normaliseDur d) (0 / 1) p False)
    fn (Rest d)         = PrimNote (Rest2 (normaliseDur d))
    s m1 m2             = case (m1, m2) of
                                 (PrimNote (Rest2 ofs), PrimNote (Note2 d ofs1 p onBeat)) -> PrimNote (Note2 (normaliseDur d) (normaliseDur ofs) p onBeat)
                                 (_,_) ->  error "condense: unexpected Music1 tree"
    p m1 m2             = -- if the sequential pair of notes are within one unitDur, assume it's a chord and just keep the last
                          -- otherwise assume it's a proper sequence and keep them both
                          if (chordalPair m1 m2) then
                            m1
                          else
                            (:+++:) m1 m2
    modify c m          = m -- throw controls away for the time being

-- Identify a pair of notes that are part of a chord - i.e. played in parallel 
-- not at all confident about this.  If the pianist plays a chord, it will confuse us so we attempt to throw the 
-- chordal notes away, just keeping the last.  At the moment, a chordal note is identified as being within
-- a single unitDur of its neighbour.
chordalPair :: Notes Prim2 -> Notes Prim2 -> Bool
chordalPair (PrimNote (Note2 _ d1 _ _)) (PrimNote (Note2 _ d2 _ _)) = let difference = abs (d1 - d2)
                                                                    in difference <= unitDur
chordalPair _ _ = False

        
midiToChar :: Midi -> AbcContext -> String
midiToChar m c = let m1 = fst3 $ fromMidi m
                     tuneHeaders = (runReader headers) c
                    in tuneHeaders ++ (flattenScore c $ 
                                       splitLongNotes $ 
                                       accidentals $ 
                                       toAbcScore c $ 
                                       tuplets c $ 
                                       numberBars $ 
                                       barline c 
                                      (condense $ removeZeros m1))

-- check the incoming midi 
checkMidi :: Midi -> Either String Midi
checkMidi m = let ft = fileType m
              in 
                if (ft == SingleTrack) then
                  Right m
                else
                  Left "Not a single track midi file"


-- load and cursorily check a midi file
loadMidiFile :: FilePath -> IO Midi
loadMidiFile fn = do
  r <- importFile fn 
  case r of
    Left err -> error err
    Right m -> do
       let mc = checkMidi m
       case mc of
         Left err -> error err
         Right m -> return m
 

fst3 (a,b,c) = a
snd3 (a,b,c) = b
thd3 (a,b,c) = c
