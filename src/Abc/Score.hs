module Abc.Score where

import Control.Monad.State

-- abstract representation of a simplified musical score
infixr 5 :+++:

data Score a = EndScore
             | Bar Int (Notes a) (Score a)
        deriving (Show, Eq, Ord)

data Notes a = PrimNote a
             | (Notes a) :+++: (Notes a)    -- a group of notes
             | Phrase (Tuplet a)            -- a duplet, triplet or quadruplet
        deriving (Show, Eq, Ord)

-- here Rational defines the type of Tuplet - 
-- (2/3) is two notes in the time of three (duplet) 
-- (3/2) is three notes in the time of two (triplet) 
-- (4/3) is four notes in the time of three (quadruplet) 
data Tuplet a = Tuplet Rational [a]
        deriving (Show, Eq, Ord)

-- a fold over Tuplets
tupFold :: (Rational -> [a] -> b) -> Tuplet a -> b
tupFold f (Tuplet d n)  =  f d n
   

-- a fold over notes
nFold ::  (a -> b) -> (b->b->b) -> (Rational -> [a] -> b) -> Notes a -> b
nFold fn (+++:) mul n =
  let rec = nFold fn (+++:) mul
  in case n of
       PrimNote ae   -> fn ae
       n1 :+++: n2   -> rec n1 +++: rec n2
       Phrase m -> tupFold mul m
 

-- a fold over scores
sFold :: b -> (Int->b->b->b) -> (a -> b) -> (b->b->b) -> (Rational -> [a] -> b) -> Score a -> b
sFold endscore fbar fn (+++:) mul s =
  let nf = nFold fn (+++:) mul 
      sf = sFold endscore fbar fn (+++:) mul 
    in case s of
      EndScore -> endscore
      Bar i ns ss -> fbar i (nf ns) (sf ss)

-- a map over the notes in tuplets
tupnMap :: (a -> b) -> Tuplet a -> Tuplet b
tupnMap f (Tuplet d n) = Tuplet d (map f n)

-- a map over all notes
nMap :: (a -> b) -> Notes a -> Notes b
nMap f (PrimNote a) = PrimNote (f a)
nMap f (n1 :+++: n2) = (nMap f n1) :+++: (nMap f n2) 
nMap f (Phrase m) = Phrase (tupnMap f m)

-- a map over notes, but picking out only the tuplets
tupMap :: (Tuplet a -> Tuplet a) -> Notes a -> Notes a
tupMap f (PrimNote a) = PrimNote a
tupMap f (n1 :+++: n2) = (tupMap f n1) :+++: (tupMap f n2) 
tupMap f (Phrase m) = Phrase (f m)


-- a map over scores
sMap :: (a -> b) -> Score a -> Score b
sMap f EndScore = EndScore
sMap f (Bar i n s) = Bar i (nMap f n) (sMap f s) 

-- reshape the notes that constitute the score without altering the type of the notes themselves
reshapeScore :: (Notes a -> Notes a) -> Score a -> Score a
reshapeScore f EndScore = EndScore
reshapeScore f (Bar i n s) = Bar i (f n) (reshapeScore f s) 

-- traverse a score and apply the function that reorganises any individual note (perhaps to a phrase)
reorgNoteInScore :: (a -> Notes a) -> Score a -> Score a
reorgNoteInScore f EndScore = EndScore
reorgNoteInScore f (Bar i n s) = Bar i (reorgIndividualNote f n) (reorgNoteInScore f s) 

-- traverse a phrase of notes and apply the note modifying function
reorgIndividualNote :: (a -> Notes a) -> Notes a -> Notes a
reorgIndividualNote f (PrimNote n) = f n
reorgIndividualNote f (n1 :+++: n2) = reorgIndividualNote f n1 :+++: reorgIndividualNote f n2
reorgIndividualNote f (Phrase m) = Phrase m  -- won't apply to tuplets

-- A monadic map over just the bars, leaving the notes unaffected
mapBarM :: Monad m => (Int -> m Int) -> Score a -> m (Score a)
mapBarM f EndScore = do
   return EndScore
mapBarM f (Bar i ns ss) = do
  bb  <- f i 
  ss' <- mapBarM f ss
  return $ Bar bb ns ss'

-- a function to number all the bars monotonically by threading state through the score
numberBarsf :: Score a -> State Int (Score a)
numberBarsf score = mapBarM number score
    where number v = do
            cur <- get
            put (cur+1)
            return cur

-- number all bars monotonically from zero
numberBars :: Score a -> Score a
numberBars s = fst $ runState (numberBarsf s) 0


