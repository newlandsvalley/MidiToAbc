-- This module is for testing purposes.  It attempts to convert a score to Euterpea music which can then be played.
-- This should allow some round-trip testing.  The final output will not be identical to the input because Euterpea's
-- original translation to midi uses a parallel succession of Rest/Note pairs which are no simplified to a simple
-- serial list of notes and also because various heuristics have been used in order to produce a comprehensible score.
-- However the tune should be immediately recognisable if it was amenable to score production in the first place. 

module Abc.Tools  ( scoreToMusic, apcTopc ) where

import Abc.Note
import Abc.Score
import Euterpea.Music.Note.Music hiding ( Mode(..), KeySig, Phrase )
import Euterpea.Music.Note.Performance ( Music1, Note1 )
import Euterpea.Music.Note.MoreMusic ( NoteAttribute(..) )
import Data.List

scoreToMusic :: AbcContext -> Score AbcEntity -> Music1
scoreToMusic c s = 
      let keyName = ctxKeyName c
          mode = ctxMode c
          ks = (keyName, mode)
      in
        scoreToMusic1 ks s

scoreToMusic1 :: KeySig -> Score AbcEntity -> Music1
scoreToMusic1 _ EndScore = Prim (Rest (0/1))
scoreToMusic1 ks (Bar n ns ss) = 
        notesToMusic ks ns :+: scoreToMusic1 ks ss

notesToMusic :: KeySig -> Notes AbcEntity -> Music1
notesToMusic ks (PrimNote n) = aeToMusic ks n
notesToMusic ks (n1 :+++: n2) = notesToMusic ks n1 :+: notesToMusic ks n2
notesToMusic ks (Phrase t) = tupletToMusic ks t

tupletToMusic :: KeySig -> Tuplet AbcEntity -> Music1
tupletToMusic ks (Tuplet d ns) = Modify (Tempo (1 / d)) $ noteListToMusic ks ns

noteListToMusic :: KeySig -> [AbcEntity] -> Music1
noteListToMusic ks (h: []) = aeToMusic ks h
noteListToMusic ks (h: t) =  aeToMusic ks h :+: noteListToMusic ks t

aeToMusic :: KeySig -> AbcEntity -> Music1
aeToMusic ks (AbcNote ap d b) = Prim (Note d (apTop ks ap, [Volume 80]))
aeToMusic ks (AbcRest d) = Prim (Rest d)
aeToMusic ks Tie = Prim (Rest (0/1))

apTop :: KeySig -> AbcPitch -> Pitch
apTop ks (pc,o) = (apcTopc ks pc, o)

apcTopc :: KeySig -> AbcPitchClass -> PitchClass
apcTopc ks apc = 
  let chromaticScale = genScale ks
      pos = elemIndex apc chromaticScale
  in case pos of
    Nothing -> error "apcTopc: note not found in scale" 
    Just n  -> lookupPC n

{-
lookupApc :: KeySig -> AbcPitchClass -> Maybe Int
lookupApc ks apc = 
  let chromaticScale = genScale ks
  in elemIndex apc chromaticScale
-}
    
lookupPC :: Int -> PitchClass
lookupPC n  = [C,Cs,D,Ds,E,F,Fs,G,Gs,A,As,B] !! n


