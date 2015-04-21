module Abc.AbcScore (flattenScore, toAbcScore, splitLongNotes, accidentals) where

-- import Euterpea
import Abc.Note
import Abc.Score
import Control.Monad.Reader
import Data.Ratio

type AbcScore = Score AbcEntity
type Accidentals = [AbcPitchClass]

-- flatten a score of Abc Entities to an array of characters
flattenScore :: AbcContext -> AbcScore -> [Char] 
flattenScore ctx score =
  let barline = "|"
      barf i x y =  -- produce a new line every barsToLine bars
                    let 
                      newl =  if (i > 0) && (i `mod` barsToLine == 0) then "\n" else ""
                    in newl ++ barline ++  x  ++ y
      ser x y = x ++ y
      disp ae = (runReader (display ae)) ctx
      tup d xs = (abcTuplet d (length xs)) ++ (concat $ map (\ae -> disp ae) xs) ++ " "
    in sFold barline barf disp ser tup score

-- an abc tuplet indicator in the form of (p:q:r - see http://www.lesession.co.uk/abc/abc_notation_part2.htm#ets
-- where
-- p = the number of notes to be put into time q
-- q = the time that p notes will be played in
-- r = the number of notes to continue to do this action for
-- simplified where possible to just p
abcTuplet :: Rational -> Int -> [Char]
abcTuplet r noteCount = 
    let numer = fromIntegral $ numerator r
    in
      if (numer == noteCount) then
         " (" ++ show numer
      else
         " (" ++ show numer ++ ":" ++ show (denominator r) ++ ":" ++ show noteCount 

-- convert a score of Prim2 notes to a score of AbcEntity
toAbcScore :: AbcContext ->  Score Prim2 ->  AbcScore
toAbcScore ctx s = 
      -- run the toAbcEntity reader on the context (for the scale) to get a function  matching pitch class to AbcEntity
      let
        toAbcE pc = (runReader (toAbcEntity pc)) ctx       
        in sMap toAbcE s

-- tuneHeaders = (runReader headers) ctx

-- split long notes
-- split any note too long to be notated singly by a tied pair of notes
splitLongNotes :: AbcScore -> AbcScore
splitLongNotes = reorgNoteInScore splitNote

-- It is not possible to notate a single note of length 5 or 7 in a conventional bar
-- these must be split up into notes that can be represented and joined with a tie
-- (an abc parser would give a 'note too dotted' error if it discovered one)
splitNote :: AbcEntity -> Notes AbcEntity 
splitNote nt@(AbcNote p d onBeat) = 
  let pair = (numerator d, denominator d) -- haskell doesn't seem to allow pattern matching on Rationals
   in
    case pair of 
      (5,8) ->  if (onBeat) then
                  (PrimNote (n1 (3 / 8)) :+++: (PrimNote Tie :+++: PrimNote (n2 (2 / 8))))
                else 
                  (PrimNote (n1 (2 / 8)) :+++: (PrimNote Tie :+++: PrimNote (n2 (3 / 8))));

      (7,8) ->  if (onBeat) then
                  (PrimNote (n1 (3 / 8)) :+++: (PrimNote Tie :+++: PrimNote (n2 (4 / 8))))
                else 
                  (PrimNote (n1 (4 / 8)) :+++: (PrimNote Tie :+++: PrimNote (n2 (3 / 8))));
                 
       _    ->  PrimNote nt;
       where 
         n1 d1 = AbcNote p d1 onBeat
         n2 d2 = AbcNote p d2 False
splitNote x = PrimNote x

-- handle accidentals properly
-- (an accidental in a bar influences other notes appearing later in the bar)
accidentals :: AbcScore -> AbcScore
accidentals = reshapeScore fixAccidentals

-- fix the acccidentals in the bar, starting with an empty list of accidentals
fixAccidentals :: Notes AbcEntity -> Notes AbcEntity
fixAccidentals ns = snd $ accidentalsWork [] ns

-- thread the state (a growing list of accidentals) through the notes in the bar
-- fixing the accidental damage as we go:
-- If the incoming note is marked as 'asKey' then change it to 'expl'  (explicit natural).
-- On the other hand, if it is not then add it to the growing list of accidentals in the bar
-- and set it to 'asKey' if there's a preceding accidental from which it inherits its characteristics
accidentalsWork :: Accidentals -> Notes AbcEntity -> (Accidentals, Notes AbcEntity)
accidentalsWork as p@(PrimNote n) =
    let (as1, n1) = fixAccidental as n
    in (as1, (PrimNote n1))
accidentalsWork as (n1 :+++: n2) =
    let (as1, ns1) = accidentalsWork as n1
        (as2, ns2) = accidentalsWork as1 n2
    in (as2, ns1 :+++: ns2)
accidentalsWork as p@(Phrase (Tuplet r ns)) = 
    let (as1, ns1) = accidentalsInTuple as ns
    in (as1, (Phrase (Tuplet r ns1)))

-- Fix the accidentals occuring in a tuple
accidentalsInTuple :: Accidentals -> [AbcEntity] -> (Accidentals, [AbcEntity])
accidentalsInTuple as [] = (as, [])  
accidentalsInTuple as (ae:aes) = 
    let (as1, ae1) = fixAccidental as ae
        (as2, aes2) = accidentalsInTuple as1 aes
    in (as2, ae1 : aes2)

-- Fix an accidental in an individual note
fixAccidental :: Accidentals -> AbcEntity -> (Accidentals, AbcEntity)
fixAccidental as ae@(AbcNote (pc, oct) dur onBeat) = 
    -- if we're marked 'as key' we make explictly natural if there's a different accidental 
    -- for this pitch previously in the bar
    if (isAsKey pc) then
       if (otherAccidentalInBar pc as) then 
         (addAccidental pc as, AbcNote (makeExplicit pc, oct) dur onBeat)
       else 
         (as, ae)
    -- if we're marked as anything else (i.e. an accidental) then  we make 'as key' natural if we've 
    -- had a matching accidental for this pitch previously in the bar to avoid double marking
    else
       if (thisAccidentalInBar pc as) then 
         (as, AbcNote (makeAsKey pc, oct) dur onBeat)
       else 
         (addAccidental pc as, AbcNote (pc, oct) dur onBeat)
fixAccidental as x = (as, x)

-- add an accidental to the list
-- Only one accidental for any given pitch (A-G) is allowed
addAccidental :: AbcPitchClass -> Accidentals -> Accidentals
addAccidental pc as = 
   let preface = pitchPreface pc
       f apc = pitchPreface apc /= preface
       cleanas = filter f as
   in pc : cleanas

-- return True if some accidental for the supplied pitch already exists in the list but 
-- which doesn't match the one we're looking for
otherAccidentalInBar :: AbcPitchClass -> Accidentals -> Bool
otherAccidentalInBar pc as =
   (someAccidentalInBar pc as) && (not $ thisAccidentalInBar pc as)

-- return True if some accidental for the supplied pitch already exists in the list
someAccidentalInBar :: AbcPitchClass -> Accidentals -> Bool
someAccidentalInBar pc as =
   let preface = pitchPreface pc    
   in preface `elem` (map pitchPreface as)

-- return True if this exact accidental for the supplied pitch already exists in the list
thisAccidentalInBar :: AbcPitchClass -> Accidentals -> Bool
thisAccidentalInBar pc as = pc `elem` as

-- return True if the pitch is marked simply as inheriting its nature from the key signature
isAsKey :: AbcPitchClass -> Bool
isAsKey a = "askey" == (tail $ show a)

-- make a pitch class explictly natural
makeExplicit :: AbcPitchClass -> AbcPitchClass
makeExplicit a = read $ pitchPreface a : "expl"

-- make a pitch class 'as key' and thus taking on the aspects of a previous accidental in the bar
makeAsKey :: AbcPitchClass -> AbcPitchClass
makeAsKey a = read $ pitchPreface a : "askey"

-- get the first letter of the accidental representing its base pitch
pitchPreface :: AbcPitchClass -> Char
pitchPreface pc = head $ show pc



