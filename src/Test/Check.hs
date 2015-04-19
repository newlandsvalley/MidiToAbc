{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Test.Check where

import Euterpea hiding ( Mode(..) )
import Abc.Note
import Test.QuickCheck
import Data.List
import Control.Monad

sharpKeys = [Csh, Dn, Dsh, En, Fsh, Gn, Gsh, An, Ash, Bn]
flatKeys =  [Dfl, Efl, Fn, Gfl, Afl, Bfl]

sharpNotes = [Cshp, Dshp, Eshp, Fshp, Gshp, Ashp, Bshp]
flatNotes = [Cflt, Dflt, Fflt, Gflt, Aflt, Bflt]
natNotes = [Cflt, Dflt, Fflt, Gflt, Aflt, Bflt]

isFlatMajorKey :: KeyName -> Bool
isFlatMajorKey kn = kn `elem` flatKeys

isSharpMajorKey :: KeyName -> Bool
isSharpMajorKey kn = kn `elem` sharpKeys

instance Arbitrary KeyName where
   arbitrary = oneof [ return Cn, return Csh, return  Dfl, return  Dn, return  Dsh, return  Efl, return  En,
                       return  Fn, return  Fsh, return  Gfl, return  Gn, return  Gsh, return  Afl, return  An, 
                       return  Ash, return  Bfl , return  Bn ]

instance Arbitrary Mode where
   arbitrary = do return Major

-- to produce arbitrary instances of a simpe type declaration, we must wrap in a newtype
-- (and then unwrap inside the property checking function) 
newtype NTimeSig = NTimeSig TimeSig
   deriving (Show)

instance Arbitrary NTimeSig where
   arbitrary = liftM NTimeSig $ oneof [ return (3,2), return (2,4), return (3,4), return (4,4), return (3,8), 
                                        return (6,8), return (9,8), return (12,8) ]

prop_roundTripDur :: Dur -> Bool
prop_roundTripDur d = normaliseDur d == (toDur $ toMeasure $ normaliseDur d)

prop_majorScale :: KeySig -> Bool
prop_majorScale k@(kn, mode) = 
    let s = genScale k
    in 
      -- confirm there are no sharpened notes in a 12-tone major flat scale
      if (isFlatMajorKey kn) then
         (s `intersect` sharpNotes) == []
      else 
     -- confirm there are no flattened notes in a 12-tone major sharp scale (or C natural scale which uses the sharp form)
         (s `intersect` flatNotes) == []

-- the number of beats is one more than the number of pulses in the bar
-- (because we record both ends in the array of pulses)
prop_numBeats :: NTimeSig -> Bool
prop_numBeats (NTimeSig t@(n,d)) =
  let pulses = if ((n `mod` 3 == 0) && (d == 8)) then 
                 n `div` 3 
               else n
  in (length $ beats t) == pulses + 1
    



