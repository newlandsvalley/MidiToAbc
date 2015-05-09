module Abc.Tuplet ( tuplets ) where


import Abc.Score
import Euterpea.Music.Note.Music ( Dur )
import Abc.Note ( Prim2 (..), AbcContext (..), TimeSig, Measure, unitDur, isTripleTime, toMeasure, measuresPerBeat, noteDisplayTolerance )
import Data.Ratio

{-
We are splitting up a standard 4/4 bar into 96 units so that a quaver (eighth note) occcupies 12 units and a crochet
(quarter note) occupies 24 units.  We use 96 because we can use it to identify individual notes within triplets and quadruplets
i.e. 3 and 4 are both factors and the unit is small enough to round the more variable Euterpea Durs without losing precision.
 
When we translate from such units to abc we will divide the duration by the noteDisplayTolerance, (currently set by dividing a sixteenth
note by the unit note duration - i.e. 1/16 by 1/96) which is 6. It is important that at all times, each note is represented as an exact 
multiple of 6 to avoid rounding/truncation errors in this translation.

The 12/8 rhythm has 12 quavers and occupies 144 measures
The 9/8  rhythm has  9 quavers and occupies 108 measures
The 6/8  rhythm has  6 quavers and occupies 72 measures

In each of these rhythms, a beat thus occupies 36 measures - an exact multiple of 6

The 2/4 rhythm has 2 crochets and occupies 48 measures
The 3/4 rhythm has 3 crochets and occupies 72 measures
The 4/4 rhythm has 4 crochets and occupies 96 measures

In each of these rhythms, a beat thus occupies 24 measures.  If such a beat is split into 3 triplets, each triplet will
occupy 8 measures which is not an exact multiple of 6.

If a triplet is notated in 12/8, 9/8 or 6/8 it thus needs no special treatment because a beat within any bar has a length which
is a multiple of 6.  If, however, it is notated in 3/4 we need to perform the following:

1) multiply the note length by 3/2
2) Encapsulate the note (and its partners) into a Triplet container which will issue the corresponding
   abc inverse operation (divide by 3/2 notated as (3:2... ) once translated to abc.
    
Thus our algorithm for triplets needs to be applied in 2/4, 3/4 and 4/4 and identify notes which are multiples of three.  It should
gather these into an individual triplet container until an exact multiple of beat-lengths (i.e. of 1/4) is reached.

Duplets are recognised by first recognising them as (4:3:2 quadruplets and then afterwards renaming them as (2 duplets.  They
appear to hold a 'magic' number of 18-measure (3/16 Dur) notes.

-}

dupletTime, tripletTime, quadrupletTime :: Rational
dupletTime = (2/3)
tripletTime = (3/2)
quadrupletTime = (4/3)

-- heuristic to identify the largest note that could fit into a tuplet
maxTupletCandidateLen :: Measure
maxTupletCandidateLen = round $ (1/6) / unitDur

-- apply tuplet recognition to all scores
tuplets :: AbcContext -> Score Prim2 -> Score Prim2
tuplets c s =  let timeSig = (ctxTimeSig c) 
                  in reshapeScore (addTuplets timeSig) s
    
-- identify all tuplet notes and gather them together within a sequence of notes
addTuplets :: TimeSig -> Notes Prim2 -> Notes Prim2
addTuplets t n = tupMap ( augmentNoteDuration . identifyDuplet ) $ toNotes $ replaceSinglets $ mergeN t $ flattenN t n


-- wrap a Prim2 note in a Tuplet container if it it a tuplet candidate
wrapNote :: Rational -> Prim2 -> Notes Prim2
wrapNote r n = if (isTupletCandidate n) then Phrase (Tuplet r [n])
               else PrimNote n

-- Flatten a phrase of music supplied as a tree of notes to an array of notes 
-- (each of which is wrapped where appropriate in a Tuplet wrapper)
-- if we're in regular time we look for triplets, if in jig time we look for quadruplets
flattenN :: TimeSig -> Notes Prim2 -> [Notes Prim2]
flattenN t p@(PrimNote n) = 
         let d = if (isTripleTime t) then
                   quadrupletTime
                 else
                   tripletTime
         in [wrapNote d n]
flattenN t ((:+++:) n1 n2) = flattenN t n1 ++ flattenN t n2
flattenN t x = error "flattenN: unrecognized Notes type"

-- merge successive tuplets together wherever possible to give a full tuplet in each case
mergeN :: TimeSig -> [Notes Prim2] -> [Notes Prim2]
mergeN t n = foldr (foldTuplets t) [] n

-- remove any degenerate tuplets consisting of only one note by reinstating the single note
-- (experimental)
replaceSinglets :: [Notes Prim2] -> [Notes Prim2]
replaceSinglets ns = let f n = case n of 
                                (Phrase (Tuplet r tns)) -> if (length tns == 1) then PrimNote (head tns) else n
                                x -> x
                    in map f ns

-- merge the next tuplet candidate note with the last one wherever possible
foldTuplets :: TimeSig -> Notes Prim2 -> [Notes Prim2] -> [Notes Prim2]
foldTuplets t n acc = case (acc, n) of
      ([], n) -> [n]
      ((Phrase m1@(Tuplet d a)):as, (Phrase m2@(Tuplet d1 n))) -> 
           if (isFullTuplet t m1) then
              (Phrase m2) : (Phrase m1) : as
           else
               Phrase (Tuplet d (n ++ a)) : as
      (x,y) -> y : x

-- unflatten the list of notes (now containing Tuplets) back to a phrase of music represented as a tree
toNotes :: [Notes Prim2] -> Notes Prim2
toNotes (x:[]) = x
toNotes (x:xs) = x :+++: toNotes xs

-- where we have a tuplet, we augment the length of the notes inside it because they eventually will be
-- prefaced by a phrase that indicates that 'p' measures will be squashed into 'q' time which results in a phrase
-- of exactly the same duration
augmentNoteDuration :: Tuplet Prim2 -> Tuplet Prim2
augmentNoteDuration (Tuplet r ns) = 
   let f = (\(Note2 d ofs p onBeat) -> (Note2 (d * r) ofs p False))
   in Tuplet r (map f ns)

-- Identify duplet (by renaming any (4:3:2 quadruplet)
identifyDuplet :: Tuplet Prim2 -> Tuplet Prim2
identifyDuplet t@(Tuplet r ns) = 
   if (r == quadrupletTime) && (length ns == 2) then
     Tuplet dupletTime ns
   else
     t

-- return True if the note is a Tuplet candidate (i.e. not a round number of displayable note durations)
-- also if the duration is 3/16 i.e. 18 measures which is the footprint of a duplet, but identified as
-- a (4:3:2 form of quadruplet
isTupletCandidate :: Prim2 -> Bool
isTupletCandidate n = 
    let base = noteDisplayTolerance
    in case n of
      (Note2 d ofs p onBeat) ->
        let m = toMeasure d	
	in (  ((m `mod` base /= 0) || ( d == (3/16))) && (m <= maxTupletCandidateLen)  )
      _ -> False


-- return True if the Tuplet is complete - i.e. holds a full set of notes
-- this will happen when the tuplet occupies a complete number of beats
isFullTuplet :: TimeSig -> Tuplet Prim2 -> Bool
isFullTuplet t n = 
  let m = toMeasure $ (tupletDuration n)
      base = measuresPerBeat t
  in m `mod` base == 0

-- calculate the overall duration of the tuplet
tupletDuration :: Tuplet Prim2 -> Dur
tupletDuration (Tuplet d ns) = 
    foldl addDur (0/1) ns
    where addDur d0 (Note2 d ofs p onBeat) = d0 + d
               

