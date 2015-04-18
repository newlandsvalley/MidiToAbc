module Abc.AbcScore where

-- import Euterpea
import Abc.Note
import Abc.Score
import Control.Monad.Reader
import Data.Ratio

type AbcScore = Score AbcEntity

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


