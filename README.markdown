MidiToAbc
=========

Phase 1
-------

This is the first phase of a project that will transform MIDI files into [ABC notation](http://abcnotation.com/). The intention is eventually to allow a MIDI keyboard to be used as an input device for generating well-formed ABC files that define the tune score. This phase concentrates particularly on tunes in the Scandi genre but has only been tested against input which was itself computer-generated (in fact from ABC files themselves) which thus have a very regular beat.  It is restricted for use with monophonic tunes that contain a single melody line within recognized traditional rhythms (jig, reel, polska etc.). 

Phase 2
-------

The next phase will be to attempt to transform MIDI files which were created directly from a midi piano. This will involve a certain measure of heuristics, because tunes produced by humans are more irregular in rhythm and don't necessarily follow a single melody line.  For example, when played legato, a note may reasonably be held for a little while after the succeeding note is pressed and so articulation towards a more machine-like performance should be attempted.  Similarly, notes may stray unintentionally and perhaps imperceptably across bar lines and will need to be cleaned up before a score can be produced. 


How to Build
------------
*  Install the Glasgow Haskell Compiler (GHC)
*  Install the dependencies
*  cd to the src directory and invoke make.sh

How to Test
-----------
The midi directory contains a set of sample Scandi tunes.  The bin directory contains a set of shell scripts which invokes MidiToAbc for each of these, supplying the correct configuration properties as command line parameters.  Output is to the abc directory. 

A midi tune can be round tripped through a Euterpea Music representation to a Score and then back to Music which can be played.  So this allows a role for your ears in the testing process.

QuickCheck tests in Test.Check are not yet complete.

Dependencies
------------

*  [Options.Applicative](https://hackage.haskell.org/package/optparse-applicative-0.1.1/docs/Options-Applicative.html).
*  [Euterpea](https://hackage.haskell.org/package/Euterpea).

Blog
----

[Elucubrations](http://myelucubrations.blogspot.co.uk/2015/04/reverse-engineering-midi.html)




