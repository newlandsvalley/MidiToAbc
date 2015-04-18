module Test.Check where

import Euterpea hiding ( Mode(..) )
import Abc.Note
import Test.QuickCheck
import Data.List

sharpKeys = [Csh, Dn, Dsh, En, Fsh, Gn, Gsh, An, Ash, Bn]
flatKeys =  [Dfl, Efl, Fn, Gfl, Afl, Bfl]


sharpNotes = [Cshp, Dshp, Eshp, Fshp, Gshp, Ashp, Bshp]
flatNotes = [Cflt, Dflt, Fflt, Gflt, Aflt, Bflt]
natNotes = [Cflt, Dflt, Fflt, Gflt, Aflt, Bflt]

isFlatMajorKey :: KeyName -> Bool
isFlatMajorKey kn = kn `elem` flatKeys

isSharpMajorKey :: KeyName -> Bool
isSharpMajorKey kn = kn `elem` sharpKeys

instance Arbitrary AbcPitchClass where
  arbitrary = oneof [return Cshp, return Dshp, return Fshp, return Gshp, return Ashp]

instance Arbitrary KeyName where
   arbitrary = oneof [ return Cn, return Csh, return  Dfl, return  Dn, return  Dsh, return  Efl, return  En,
                       return  Fn, return  Fsh, return  Gfl, return  Gn, return  Gsh, return  Afl, return  An, 
                       return  Ash, return  Bfl , return  Bn ]

instance Arbitrary Mode where
   arbitrary = do return Major

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
    



