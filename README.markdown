MidiToAbc
=========

This is the first phase of a project that will transform midi files into [ABC notation](http://abcnotation.com/). The intention is eventually to allow a midi keyboard to be used as an input device for generating well-formed ABC files that define the tune score. This phase concentrates particularly on tunes in the Scandi genre but has only been tested against midi input which was itself computer-generated (in fact from ABC files themselves) which thus have a very regular beat.  It is restricted for use with monophonic tunes that contain a single melody line within recognized traditional rhythms (jig, reel, polska etc.). 

The next phase will be to attempt to transform midi files which were created directly from a midi piano.


How to Build
------------
*  Install the Glasgow Haskell Compiler (GHC)
*  Install the dependencies
*  cd to the src directory and invoke make.sh

How to Test
-----------
The midi directory contains a set of sample Scandi tunes.  The bin directory contains a set of shell scripts which invokes MidiToAbc for each of these, supplying the correct configuration properties as command line parameters.  Output it to the abc directory. 

As yet, there are no unit tests.

Dependencies
------------

*  [Options.Applicative](https://hackage.haskell.org/package/optparse-applicative-0.1.1/docs/Options-Applicative.html).
*  [Euterpea](https://hackage.haskell.org/package/Euterpea).

Blog
----

[Elucubrations](http://myelucubrations.blogspot.co.uk/)




