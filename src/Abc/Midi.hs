module Abc.Midi (barline, condense, midiToChar, midiToScore, loadMidiTrack, firstTimeSig, extractTimeSigs, testTS, fst3, snd3, thd3) where

import Euterpea
import Abc.Note
import Abc.Score
import Abc.AbcScore
import Abc.Metadata
import Abc.Tuplet
import Codec.Midi
import Control.Monad.Reader
import Data.List

type IsBarLine = Bool
type IsTie = Bool

-- Work out if a bar line and/or a tie needs to be generated for the next note
-- the incoming parameters are the context and the durations of the note and its offset in a tune
-- the result is a 4-tuple indicating if a bar line is needed, whethere the note lies on the beat
-- and finally indicating the left hand and right hand note durations if the note is tied
-- across the bar 
detectBarline :: AbcContext -> Dur -> Dur -> (IsBarLine, OnBeat, Maybe Dur, Maybe Dur)
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
                spaceUsed = bar - spaceRemaining
                onBeat = elemIndex spaceUsed beats
             in 
                -- there's not enough room for the next note after this so generate a bar
                if ((spaceRemaining < (note + toMeasure shortestSupportedNote)) && (spaceRemaining >= note)) then
                  (True, onBeat, Just noteDur, Nothing)
                -- there's plenty of room for this note so no bar is necessary
                else if (note <= spaceRemaining) then
                  (False, onBeat, Just noteDur, Nothing)
                -- otherwise we may need a tie
                else 
                  let lh = spaceRemaining
                      rh = note - spaceRemaining
                  in
                    -- the left hand of the tie is not big enough to display
                    if (lh < toMeasure shortestSupportedNote) then 
                      (True, Nothing,  Nothing, Just (toDur rh)) 
                    -- the right hand of the tie is not big enough to display
                    else if (rh < toMeasure shortestSupportedNote) then 
                      (True, onBeat,  Just (toDur lh), Nothing )
                    -- otherwise a genuine tie
                    else
                      (True, onBeat, Just (toDur lh),  Just (toDur rh))


-- create a score of Prim2 notes taking account of bars, notes tied across bars and beats in the bar.
-- this inserts Bar nodes into the tree
-- Bars are unnumbered at this stage
barline :: AbcContext -> Notes Prim2 -> Score Prim2
barline c n = nFold fn s mul n where
     fn nt@(Note2 d ofs p onBeat) =  Bar 0 (PrimNote nt) EndScore -- all we can do is return Bar for a primitive although these are not yet fully formed

     fn EmptyNote = Bar 0 (PrimNote EmptyNote) EndScore
     
     fn r@(Rest2 d ofs) = Bar 0 (PrimNote r) EndScore 
     
     mul d ms = error "barline: unexpected multiplet" 
     
     s (Bar _ (PrimNote n1@(Note2 d ofs p _)) next1) (Bar _ n2 next2) =  
             -- n1 is the next incoming note to be attached to the rest of the tree
             let (isBarLine, isOnBeat, maybeLeft, maybeRight) = detectBarline c d ofs
                 newn1 = PrimNote (Note2 d ofs p isOnBeat) -- newn1 is identical to n1 but with the beat now detected
                            in if (isBarLine) then
                                 case ((maybeLeft, maybeRight)) of
                                      (Just l, Just r) ->
                                          let 
                                             lhNote = PrimNote (Note2 l ofs p isOnBeat)
                                             rhNote = PrimNote (Note2 r (ofs + l) p Nothing)
                                          in
                                            Bar 0 lhNote (Bar 0 (PrimNote TiedNote :+++: rhNote :+++: n2) next2)   
                                            
                                      (Just l, Nothing) -> 
                                          let 
                                             lhNote = PrimNote (Note2 l ofs p isOnBeat)
                                          in
                                            Bar 0 lhNote (Bar 0 n2 next2)     
                                            
                                      (Nothing, Just r) ->  
                                          let 
                                            rhNote = PrimNote (Note2 r ofs p Nothing)
                                          in
                                            Bar 0 (PrimNote EmptyNote) (Bar 0 (rhNote :+++: n2) next2)  ;  
                                            
                                      (_,_) ->  
                                          error "Barline: detecting tie - no notes"           
                                 else
                                       Bar 0 (newn1 :+++: n2)  next2;       
                                       
     s (Bar _ (PrimNote n1@(Rest2 d ofs )) next1) (Bar _ n2 next2) =  
             -- n1 is the next incoming Rest to be attached to the rest of the tree
             let (isBarLine, isOnBeat, maybeLeft, maybeRight) = detectBarline c d ofs
                            in if (isBarLine) then
                                 case ((maybeLeft, maybeRight)) of
                                      (Just l, Just r) ->
                                          let 
                                             lhRest = PrimNote (Rest2 l ofs)
                                             rhRest = PrimNote (Rest2 r (ofs + l))
                                          in
                                            Bar 0 lhRest (Bar 0 (rhRest :+++: n2) next2)   
                                            
                                      (Just l, Nothing) -> 
                                          let 
                                             lhRest = PrimNote (Rest2 l ofs)
                                          in
                                            Bar 0 lhRest (Bar 0 n2 next2)     
                                            
                                      (Nothing, Just r) ->  
                                          let 
                                            rhRest = PrimNote (Rest2 r ofs)
                                          in
                                            Bar 0 (PrimNote EmptyNote) (Bar 0 (rhRest :+++: n2) next2)  ;  
                                            
                                      (_,_) ->  
                                          error "Barline: detecting split rests - no rests"           
                                 else
                                       Bar 0 (PrimNote n1 :+++: n2)  next2;          
     
       s x y = error "barline: unexpected node in Notes Prim2 tree"



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
    fn (Note d (p,v))   = PrimNote (Note2 (normaliseDur d) (0 / 1) p Nothing)
    fn (Rest d)         = PrimNote (Rest2 (normaliseDur d) (0 / 1))
    s m1 m2             = case (m1, m2) of
                                 (PrimNote (Rest2 ofs _), PrimNote (Note2 d ofs1 p onBeat)) -> PrimNote (Note2 (normaliseDur d) (normaliseDur ofs) p onBeat)
                                 (_,_) ->  error "condense: unexpected Music1 tree"
    p m1 m2             = (:+++:) m1 m2
    modify c m          = m -- throw controls away for the time being
    
-- return True if we detect any note at all in the Music
foundANote :: Music1 -> Bool
foundANote = mFold fn s p modify where
    fn (Note d (p,v))   = True
    fn (Rest d)         = False
    s m1 m2             = m1 || m2
    p m1 m2             = m1 || m2
    modify c m          = m


-- the main transformational routine from midi to a textual score       
midiToChar :: Midi -> AbcContext -> String
midiToChar m c = let m1 = fst3 $ fromMidi m
                     tuneHeaders = (runReader headers) c
                    in 
                      if (foundANote m1) then                    
                         tuneHeaders ++ (flattenScore c $ 
                                         splitLongNotes $ 
                                         accidentals $ 
                                         toAbcScore c $ 
                                         tuplets c $ 
                                         numberBars $ 
                                         barline c 
                                         (articulate . condense $ removeZeros m1))
                      else
                         error $ "No melody detected in track "  ++ show (ctxTrackNo c)

-- transformational routine used in round-trips
midiToScore :: Midi -> AbcContext -> Score AbcEntity
midiToScore m c = let m1 = fst3 $ fromMidi m
                           in splitLongNotes $ 
                              accidentals $ 
                              toAbcScore c $ 
                              tuplets c $ 
                              numberBars $ 
                              barline c 
                              (articulate . condense $ removeZeros m1)

-- experimental methods for investigating midi metadata

-- load a midi file and get at the track with the melody together with the time signature
-- and fix any NoteOn messages with velocity 0 as NoteOff
loadMidiTrack :: Int -> FilePath -> IO (Midi, TimeSig)
loadMidiTrack tno fn  = do
  r <- importFile fn 
  case r of
    Left err -> error err
    Right m -> do
       let mc = getMidiTrack m tno
           ts = trackZeroTimeSig m
       case mc of
         Left err -> error err
         Right m -> return (ensureNoteOffs m, ts)

-- load a midi file
loadMidiFile fn = do
   r <- importFile fn 
   case r of
     Left err -> error err
     Right m  -> return m

-- experimental routine to fix midi files that use NoteOn with a velocity of 0 as an alternative to NoteOff
ensureNoteOffs :: Midi -> Midi
ensureNoteOffs (Midi tp td trks) = (Midi tp td (map noteOffs trks))
   where
     noteOffs :: Track Ticks -> Track Ticks
     noteOffs t = map makeNoteOff t 

-- convert a NoteOn velocity 0 message to NoteOff
makeNoteOff :: (Ticks, Message) -> (Ticks, Message)
makeNoteOff n@(ticks, NoteOn {channel = c, key = k, velocity = v}) = 
  if (v == 0) then
    (ticks, NoteOff {channel = c, key = k, velocity = v})
  else
    n
makeNoteOff x = x

-- get the incoming midi track 
getMidiTrack :: Midi -> Int -> Either String Midi
getMidiTrack m tno = let ft = fileType m
              in 
                if (ft == SingleTrack) then
                  Right m
                else if (ft == MultiTrack) then
                  toMonophonic m tno
                else
                  Left "Multi pattern midi not supported"


-- convert a multi-track Midi tune to a Single Track tune by filtering just the required track number
toMonophonic :: Midi -> Int -> Either String Midi
toMonophonic  m@(Midi SingleTrack _ _) _  = Right m
toMonophonic  (Midi MultiTrack td trks) tno = 
  if (tno < 0 || tno >= length trks) then
    Left $ "Track number " ++ show tno ++ " not found"
  else
    Right (Midi SingleTrack td [trks !! tno])
toMonophonic  (Midi MultiPattern td trks) tno = Left "Multi pattern midi not supported"

 
-- extract any time signatures from the initial track metadata and return in the form of an ABC Tune time signature
initialTimeSigs :: Track a -> [TimeSig]
initialTimeSigs t =
    let headers = takeWhile (\(a,m) -> isMetaMessage m ) t
        tsigms = filter (\(a,m) -> case m of
             (TimeSignature _ _ _ _) -> True
             _ -> False
            ) headers
    in toTuneTimeSig tsigms


-- convert HCodec TimeSignatures from track metadata to ABC Tune TimeSigs
toTuneTimeSig :: Track a -> [TimeSig]
toTuneTimeSig t = map toTS t
                where
                 toTS  (a,(TimeSignature n d _ _)) = (n, 2^d)
                 toTS _ = error "midi header initialTimeSigs fillter failed"


-- extract all the time signatures in the midi file and return as a list of lists
-- (where each internal list holds the initial time signatures for each track)
extractTimeSigs :: Midi -> [[TimeSig]]
extractTimeSigs (Midi _ _ trks) = map initialTimeSigs trks


{- a couple of heuristics for extracting the time signature.
   (no real idea which one to use yet)
-}

-- extract the final time signature for track zero
trackZeroTimeSig :: Midi -> TimeSig
trackZeroTimeSig m = let 
                   tzsigs = head $ extractTimeSigs m
                   in case tzsigs of
                      ([]) -> error "No time signature found in midi" 
                      x -> head $ reverse x
          
-- extract the first we come across
firstTimeSig :: Midi -> TimeSig 
firstTimeSig m = let 
                   tsigs = join $ extractTimeSigs m
                   in case tsigs of
                      ([]) -> error "No time signature found in midi track zero"
                      (x:xs) -> x          
           
      

testTS fn= do
           x <- loadMidiFile fn
           print $ extractTimeSigs x




fst3 (a,b,c) = a
snd3 (a,b,c) = b
thd3 (a,b,c) = c
