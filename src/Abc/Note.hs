-- This module represents a low level Music Primitive Entity in ABC-friendly format
-- with functions to convert from a Euterpea Primitive

-- An ABC friendly Pitch Class. expl means that a note must be explicitly marked as natural
-- in the context of a key signature where it's either sharpened or flattened.
-- askey means that the pitch inherits its sharp or flat nature from the key signature.
-- (hence all naturals are askey in the key of C Major)

module Abc.Note ( AbcPitch, AbcScale, Keys, AbcContext (..), AbcEntity (..), AbcPitchClass (..), Prim2 (..), 
                           Mode (..), KeyName (..), Rhythm (..), KeySig, TimeSig, OnBeat, Measure, BPM,
                           toAbcEntity,  display, toMeasure, toDur, normaliseDur, measuresPerBar, 
                           measuresPerBeat, unitDur, barsToLine, noteDisplayTolerance, shortestSupportedNote, shortestSupportedRest,
                           tsToDur, beats, keys, genScale, isTripleTime, displayRhythm ) where

import Euterpea.Music.Note.Music hiding ( Mode(..), KeySig )
import Data.Char
import Data.Maybe
import Control.Monad.Reader


data AbcPitchClass  =   Cflt | Cexpl | Caskey | Cshp | Dflt  | Dexpl | Daskey  | Dshp 
                 |  Eflt | Eexpl | Easkey | Eshp | Fflt | Fexpl | Faskey | Fshp
                 |  Gflt  | Gexpl | Gaskey | Gshp | Aflt |  Aexpl | Aaskey | Ashp 
                 |  Bflt | Bexpl | Baskey | Bshp 
     deriving (Show, Eq, Ord, Read, Enum, Bounded)

-- a Measure is a note duration as a rounded number of Euterpea Dur (fractional durations) 
type Measure = Int

-- Beats per minute
type BPM = Int

-- does the note occur on the beat
type OnBeat = Maybe Int

-- An ABC pitch cf Euterpea Pitch (Euterpea Octave is a type synonym for Integer where 4 is the octave of middle C)
type AbcPitch = (AbcPitchClass, Octave)

-- An ABC entity at the note level
data AbcEntity = AbcNote AbcPitch Dur OnBeat
                 | AbcRest Dur
                 | Tie
		 | AbcEmptyNote
        deriving (Show, Eq, Ord)


-- Identical to Euterpea Mode but unfortunately that didn't derive Show
data Mode = Major | Minor
  deriving (Show, Eq, Ord, Read, Enum, Bounded)

-- supported rhythms
data Rhythm =
   Jig | SlipJig | Slide | Reel | Polka | Hornpipe | Barndance | Highland | March | Mazurka | 
   Polska | Brudmarsch | Marsch | Slängpolska| Waltz | Långdans| Schottis | Engelska | Gånglåt | Hambo | Skänklåt
     deriving (Show, Eq, Ord, Read)

-- any scale but used here particularly to represent different chromatic scales
type AbcScale = [AbcPitchClass]
-- any set of pitches defining a key - must be all sharps or all flats. The empty set is they key of C.
type Keys = [AbcPitchClass] 

type KeyMap = (AbcPitchClass, Keys)

-- Mnemonic names of supported keys
data KeyName =  Cn | Csh | Dfl | Dn | Dsh | Efl | En | Fn | Fsh | Gfl | Gn | Gsh | Afl | An | Ash | Bfl | Bn  
     deriving (Eq, Ord, Read, Enum, Bounded)

-- this table indicates how key names are displayed in AbcNotation - where Keys in Key Signatures
-- have a different notation from those representing notes in a scale
instance Show KeyName where
  show Cn    = "C"
  show Csh   = "C#"
  show Dfl   = "Db"
  show Dn    = "D"
  show Dsh   = "D#"
  show Efl   = "Eb"
  show En    = "E"
  show Fn    = "F"
  show Fsh   = "F#"
  show Gfl   = "Gb"
  show Gn    = "G"
  show Gsh   = "G#"
  show Afl   = "Ab"
  show An    = "A"
  show Ash   = "A#"
  show Bfl   = "Bb"
  show Bn    = "B"

-- a key signature is a Key Name (e.g. C) and an accompanying Mode  (e.g. Major or Minor - later we'll add Mixolydian etc)
type KeySig = (KeyName, Mode)  

-- a time signature
type TimeSig = (Int, Int)

-- but often better represented as a Rational
tsToDur :: TimeSig -> Dur
tsToDur (n, d) = fromIntegral n / fromIntegral d 

-- is the time signature in triple time?
isTripleTime :: TimeSig -> Bool
isTripleTime ts = ts `elem` [(3,8), (6,8), (9,8), (12,8)]

-- how many measures of unitDur are within a bar of the supplied time signature
measuresPerBar :: TimeSig -> Int
measuresPerBar ts =  round $ (tsToDur ts) / unitDur

-- how many measures of unitDur are within a beat of the supplied time signature
measuresPerBeat :: TimeSig -> Int
measuresPerBeat ts@(n,d) = let grouping = if (isTripleTime ts) then 3 else 1
                           in
                             (measuresPerBar ts) * grouping `div` n

-- the context for generating an ABC Score
data AbcContext = AbcContext {
                    ctxTrackNo        :: Int,       -- the midi track number 
                    ctxName           :: String,    -- the tune name
                    ctxRhythm         :: Rhythm,    -- the name of the rhythm (e.g. polska)
                    ctxKeyName        :: KeyName,   -- the Key Name (e.g. An)
                    ctxMode           :: Mode,      -- the Mode (Major | Minor)
                    ctxScale          :: AbcScale,  -- the scale that defines the key signature
                    ctxLeadIn         :: Dur,       -- the lead-in bar or 'pickup' duration
                    ctxTimeSig        :: TimeSig,   -- the time signature
                    ctxBPM            :: BPM,       -- the beats per minute
                    ctxDefaultNoteLen :: Dur,       -- the duration of a single note  (ABC L field)
                    ctxBeats          :: [Int]      -- Measures at which the beats occur in this rhythm
                  }

-- a data type used just to represent notes immediately after translation from MIDI
-- the first dur is the note length, the second is the note's offset into the tune
-- the boolean value detects whether the note occurs on a regular beat of the bar
data Prim2 =  Note2 Dur Dur Pitch OnBeat
             | Rest2 Dur Dur   
             | TiedNote    
	     | EmptyNote
        deriving (Show, Eq, Ord)

-- Euterpea uses fractional durations but ABC uses integral durations.  Let's unify on a smallest detectable
-- note duration of 1/96th note.  This I hope is convenient because:
--  a) It is an exact multiple of 1/16 and of 1/8 which are the probable target ABC default note lengths (L field) to use
--     and will be a convenient multiple for most simple rhythms such as 3/4 or 4/4
--  b) It is an exact multiple of 1/3 and therefore suitable for complex rhythms such as 6/8 or 9/8
--  c) Hopefully we an use it to identify triplet polskas written in 3/4 where each triplet will occupy
--     24 units and so each note in the triplet occupies 8 units
--  d) We can also use it to identify syncopated patterns in complex rhythms, for example in a quadruplet,
--     each note would occupy 6 units (see noteDisplayTolerance)
unitDur :: Dur
unitDur = 1 / 96

-- how many bars do we show on a line
barsToLine :: Int
barsToLine = 6

-- what's the smallest unit of note measure we can use without truncation or rounding errors when converting to abc
-- This is currently set to a sixteenth note divided by the unitDur - i.e. 1/16 / 1/96 or 6.
noteDisplayTolerance :: Measure
noteDisplayTolerance = round $ sn / unitDur

-- what's the shortest supported note that we can actually display ?  Currently we use a thirty-second note - tn
-- and what about the shortest rest we can support - we may get better results from live performances by making this bigger
shortestSupportedNote, shortestSupportedRest :: Dur
shortestSupportedNote = tn
shortestSupportedRest = sn

-- Work out the measures at which the beats occur in a bar of music of any given time signature
-- We are currently using a unit Duration of 1/96 which means a bar of (4,4) occupies 96, (3,4) occupies 72,
-- (9,8) occupies 108 units etc.  beats returns a list of measures of unit durations at which the beats occur 
--
-- for example:
-- (3,4) ->  [0,24,48]
-- (4,4) ->  [0,24,48,72]
-- (6,8) ->  [0,36]
-- (9,8) ->  [0,36,72]  etc.
beats :: TimeSig -> [Int]
beats ts@(n,d) = let pulse = measuresPerBeat ts 
                     max   = measuresPerBar ts 
                 in takeWhile ( <= max) (iterate (\x -> x + pulse) 0)

-- convert a Euterpea duration to an ABC measure (Euterpea Dur is simply a type synonym for Rational)
toMeasure :: Dur -> Measure
toMeasure d = round $ d / unitDur

-- convert a Euterpea duration to an ABC measure (Euterpea Dur is simply a type synonym for Rational)
toDur :: Measure -> Dur 
toDur m = (fromIntegral m) * unitDur

-- normalise a duration to an exact multiple of unitDur
normaliseDur :: Dur -> Dur
normaliseDur d = toDur $ toMeasure d
              


-- we use the sharpened scale as the default scale
cScale, cScaleFlat :: AbcScale
cScale = [Caskey,Cshp,Daskey,Dshp,Easkey,Faskey,Fshp,Gaskey,Gshp,Aaskey,Ashp,Baskey]
cScaleFlat = [Caskey,Dflt,Daskey,Eflt,Easkey,Faskey,Gflt,Gaskey,Aflt,Aaskey,Bflt,Baskey]

sharpKeys = filter (\k  -> (k /= Faskey)) cScale
flatKeys  = [Caskey,Dflt,Eflt,Faskey,Gflt,Aflt,Bflt]

-- translate a key name to an Abc Pitch Class
abcPitchClass :: KeyName -> AbcPitchClass
abcPitchClass kn = case kn of
      Cn    -> Caskey
      Csh   -> Cshp
      Dfl   -> Dflt
      Dn    -> Daskey
      Dsh   -> Dshp
      Efl   -> Eflt
      En    -> Easkey
      Fn    -> Faskey
      Fsh   -> Fshp
      Gfl   -> Gflt
      Gn    -> Gaskey
      Gsh   -> Gshp
      Afl   -> Aflt
      An    -> Aaskey
      Ash   -> Ashp
      Bfl   -> Bflt
      Bn    -> Baskey

-- return the name of the major key associated with the supplied minor key
relativeMajor :: KeyName -> KeyName
relativeMajor kn = case kn of
      Cn    -> Efl
      Csh   -> En
      Dfl   -> En -- doesn't exist
      Dn    -> Fn
      Dsh   -> Fsh
      Efl   -> Gfl
      En    -> Gn
      Fn    -> Afl
      Fsh   -> An
      Gfl   -> An -- doesn't exist
      Gn    -> Bfl
      Gsh   -> Bn
      Afl   -> Bn -- we're not using C♭
      An    -> Cn
      Ash   -> Csh
      Bfl   -> Dfl
      Bn    -> Dn

-- circular successor
csucc :: (Enum a, Ord a, Bounded a) => a -> a
csucc a = if (a == maxBound) then 
            minBound
          else
            succ a

-- circular predecessor
cpred :: (Enum a, Ord a, Bounded a) => a -> a
cpred a = if (a == minBound) then 
            maxBound
          else
            pred a


-- translate an ABC pitch in the context of a sharp key signature so that we use askey or explicit pitches
-- e.g. if G# in the key signature, translate G# as Gaskey (i.e. in the context of a G#),
-- G as Gexplicit (i.e. an explicit natural G overriding the key signature) 
translatePitchSharp :: Keys -> AbcPitchClass -> AbcPitchClass
translatePitchSharp keys apc =  if (apc  `elem` keys) then cpred apc
                                  else if ( csucc apc  `elem` keys) then cpred apc
                                  else apc

-- translate an ABC pitch in the context of a flat key signature so that we use askey or explicit pitches
-- e.g. if G♭ in the key signature, translate  G♭ as Gaskey (i.e. in the context of a  G♭),
-- G as Gexplicit (i.e. an explicit natural G overriding the key signature) 
translatePitchFlat :: Keys -> AbcPitchClass -> AbcPitchClass
translatePitchFlat keys apc =  if (apc  `elem` keys) then csucc (csucc apc)
                                  else if ( cpred (cpred apc)  `elem` keys) then cpred apc
                                  else apc

-- generate a scale corresponding to the requested key signature
genScale :: KeySig -> AbcScale
genScale (kn, m) =
   let keyName = if (m == Major) then kn else relativeMajor kn
       abcpc = abcPitchClass keyName
   in genMajorScale abcpc
     
 

-- generate a major scale (in abc-friendly pitch notation) from the supplied pitch class defining the key 
genMajorScale :: AbcPitchClass -> AbcScale
genMajorScale target = if (target == Caskey) then cScale  -- default to sharp scale for C natural
                       else if (target `elem` sharpKeys) then 
                         map (translatePitchSharp $ genMajorKeys target) cScale
                       else
                         map (translatePitchFlat $ genMajorKeys target) cScaleFlat
                         

-- travel clockwise once round the circle of fifths and append the next sharp pitch to the key signature
nextSharpKeyMap :: (Int, KeyMap) -> (Int, KeyMap)
nextSharpKeyMap (position, ks) = let (apc, keys) = ks
                                     nextPos = (position + 7) `mod` 12
                                     nextPitch = cScale !! nextPos
                                     nextSharp = cScale !! (nextPos -1)
                                 in (nextPos, (nextPitch, nextSharp : keys))

-- travel anticlockwise once round the circle of fifths and append the next flat pitch to the key signature
nextFlatKeyMap :: (Int, KeyMap) -> (Int, KeyMap)
nextFlatKeyMap (position, ks) = let  (apc, keys) = ks
                                     nextPos = abs $ (position - 7) `mod` 12
                                     flatPos = abs $ (position - 2) `mod` 12
                                     nextPitch = cScaleFlat !! nextPos
                                     nextFlat  = cScaleFlat !!  flatPos
                                 in (nextPos, (nextPitch, nextFlat : keys))

-- generate a major key signature (a set of sharpened or flattened pitches) from a Key pitch
genMajorKeys :: AbcPitchClass ->  Keys
genMajorKeys target = if (target == Caskey) then []
                   else if (target `elem` sharpKeys) then 
                     let (pos, (k, ks)) = genSharpPitches target (0, (Caskey, []))
                        in ks
                   else
                     let (pos, (k, ks)) = genFlatPitches target (0, (Caskey, []))
                        in ks

-- get the keys from a key signature
keys :: KeySig -> Keys
keys (kn,m) = case m of
   Major -> genMajorKeys $ abcPitchClass kn
   Minor -> genMajorKeys $ abcPitchClass $ relativeMajor kn

-- generate the set of sharpened pitches in KeyMap that represent the requested Pitch class by traversing the cycle of fifths
-- The Int parameter represents the position in the clockwise cycle of fifths (i.e. cScale position modulo 12)
genSharpPitches :: AbcPitchClass -> (Int, KeyMap) -> (Int, KeyMap)
genSharpPitches target k@(pos, (pitch, ks)) = if (target == pitch) then k
                                         else genSharpPitches target $ nextSharpKeyMap k

-- generate the set of flattened pitches in KeyMap that represent the requested Pitch class by traversing the cycle of fifths
-- The Int parameter represents the position in the anticlockwise cycle of fifths (i.e. cScaleFlat position modulo 12)
genFlatPitches :: AbcPitchClass -> (Int, KeyMap) -> (Int, KeyMap)
genFlatPitches target k@(pos, (pitch, ks)) = if (target == pitch) then k
                                         else genFlatPitches target $ nextFlatKeyMap k



-- translate a Euterpea pitch to a reader from the context to an AbcPitch
toAbcPitch :: Pitch -> Reader AbcContext AbcPitch
toAbcPitch (pc,o) = do
                       scale <- asks ctxScale
                       let pos = pcToInt pc
                         in return (scale !! pos, o)

-- translate a Euterpea Music pitch to a reader from the context to an  AbcEntity
toAbcEntityMP :: Music Pitch ->  Reader AbcContext AbcEntity
toAbcEntityMP (Prim (Note dur (pc,o))) = 
         do
            scale <- asks ctxScale
            let pos = pcToInt pc                
              in return $ AbcNote (scale !! pos, o) dur Nothing
toAbcEntityMP (Prim (Rest dur))  =  do return $ AbcRest dur

-- translate a Prim2 value to a reader from the context to an  AbcEntity
toAbcEntity :: Prim2 ->  Reader AbcContext AbcEntity
toAbcEntity (Note2 durn durt (pc,o) onBeat) = 
         do
            scale <- asks ctxScale
            let pos = pcToInt pc                
              in return $ AbcNote (scale !! pos, o) durn onBeat
toAbcEntity (Rest2 durn durt)  =  do return $ AbcRest durn
toAbcEntity TiedNote  =  do return Tie
toAbcEntity EmptyNote  =  do return AbcEmptyNote

-- display an Octave in ABC format
displayOct :: Octave -> [Char]
displayOct 1 = ",,,"
displayOct 2 = ",,"
displayOct 3 = ","
displayOct 4 = ""
displayOct 5 = ""
displayOct 6 = "'"
displayOct 7 = "''"
displayOct 8 = "'''"

-- display a Pitch Class in (upper case) ABC format
displayPC :: AbcPitchClass -> [Char]
displayPC Cflt   = "_C"
displayPC Cexpl  = "=C"
displayPC Caskey = "C"
displayPC Cshp   = "^C"
displayPC Dflt   = "_D"
displayPC Dexpl  = "=D"
displayPC Daskey = "D"
displayPC Dshp   = "^D"
displayPC Eflt   = "_E"
displayPC Eexpl  = "=E"
displayPC Easkey = "E"
displayPC Fflt   = "_F"
displayPC Eshp   = "^E"
displayPC Fexpl  = "=F"
displayPC Faskey = "F"
displayPC Fshp   = "^F"
displayPC Gflt   = "_G"
displayPC Gexpl  = "=G"
displayPC Gaskey = "G"
displayPC Gshp   = "^G"
displayPC Aflt   = "_A"
displayPC Aexpl  = "=A"
displayPC Aaskey = "A"
displayPC Ashp   = "^A"
displayPC Bflt   = "_B"
displayPC Bexpl  = "=B"
displayPC Baskey = "B"
displayPC Bshp   = "^B" 

-- display an ABC Entity (note, rest or bar line) in ABC format
-- display returns a context reader because the display of a given note depends on the default note length
-- there seems to be a bug in Euterpea which is strange of an off by one error in octave
display :: AbcEntity -> Reader AbcContext [Char]
display (AbcNote (pc, buggyoct) dur onBeat) = 
                          do 
                             duration <- displayDur dur
                             padding <- displayPadding onBeat
                             let oct = buggyoct - 1  
                               in if (oct <= 4) then
                                     return $ padding ++ displayPC pc ++ displayOct oct ++ duration 
                                   else
                                     -- higher pitches than middle C are represented in lower case
                                     return $ padding ++ (map toLower (displayPC pc)) ++ displayOct oct ++ duration
display (AbcRest dur) = do
                          duration <- displayDur dur
                          return $ " z" ++ duration ++ " "
display Tie = do return "-"
display AbcEmptyNote = do return ""

-- Display the note-separation padding determined by the beat 
-- (where a beat is either None or Just x where x is the beat number counting from 0)
--
-- This attempts to sort out beams in the printed score.  ABC tries to beam together notes which are adjacent
-- but does not do so if notes are separated by a space.  Our space padding algorithm does the following:
--   note not on beat - no padding
--   note on beat (all time signatures except 4/4) - pad
--   note on beat 4/4 - pad on even numbered beats
displayPadding :: OnBeat -> Reader AbcContext [Char]
displayPadding onBeat = 
   do  
     ts <- asks ctxTimeSig
     let padding = if (isNothing onBeat) then 
                      ""
                   else if (ts == (4,4)) then case onBeat of
                      (Just 0) -> " "
                      (Just 2) -> " "
                      _ -> "";
                   else
                      " "
     return padding

-- Display a duration as an ABC measure.  For any default note length dfn
-- Notes no smaller than dfn display as 1, 2, 4 etc  (although 1 is ignored as the default ABC value)
-- Notes smaller than dfn display as  /2, /4 etc
displayDur :: Dur -> Reader AbcContext [Char]
displayDur d = do
                 defaultNoteLen <- asks ctxDefaultNoteLen     
                 let fractional = round (defaultNoteLen / d) -- the fractional note multiplication for notes smaller than the default
                     integral = round (d / defaultNoteLen)   -- the integral note multiplication for notes bigger than or equal to the default
                   in 
                     if (fractional > 1) then 
                       return $  "/" ++ show fractional
                     else
                       -- don't bother displaying notes of unit length
                      return $ if (integral > 1) then show integral else ""

displayRhythm :: Rhythm -> String
displayRhythm SlipJig = "Slip jig"
displayRhythm r = show r 

testTranslatePitches :: [AbcPitch]
testTranslatePitches = let context = AbcContext {ctxTrackNo = 0,
                                                 ctxName = "any",
                                                 ctxRhythm = Reel,
                                                 ctxKeyName = Gn,
                                                 ctxMode = Major,
                                                 ctxScale = genScale (Gn, Major),
                                                 ctxLeadIn = (0 / 16),
                                                 ctxTimeSig = (4,4),
                                                 ctxBPM = 120,
                                                 ctxDefaultNoteLen = (1 / 16),
                                                 ctxBeats = beats (4,4) }
                           fn pc = (runReader (toAbcPitch pc)) context
                             in map fn [(C,4),(D,4),(E,4),(F,4),(Fs,4),(G,4),(A,4)]
                             

-- translate a list of Euterpea notes to a list of ABC strings
testTranslateNotes :: [[Char]]
testTranslateNotes = let context = AbcContext {ctxTrackNo = 0,
                                               ctxName = "any",
                                               ctxRhythm = Waltz,
                                               ctxKeyName = Dn,
                                               ctxMode = Major,
                                               ctxScale = genScale (Gn, Major),
                                               ctxLeadIn = (0 / 16),
                                               ctxTimeSig = (3,4),
                                               ctxBPM = 120,
                                               ctxDefaultNoteLen = (1 / 16),
                                               ctxBeats = beats (3,4) }
                         -- run the toAbcEntity reader on the context (for the scale) to get a function  matching pitch class to AbcEntity
                         fn pc = (runReader (toAbcEntityMP pc)) context
                         -- run the display reader on the context (for the default note length) to get a function  matching AbcEntity to text
                         disp ae = (runReader (display ae)) context
                         abcEntities = map fn [b 3 qn, b 4 qn, c 5 en,d 5 en,e 5 sn,f 5 sn, fs 5 tn,g 5 tn, a 5 en, rest en, a 6 en]
                           in map disp abcEntities
