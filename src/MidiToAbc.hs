import Options.Applicative
import Euterpea ( Dur )
import Abc.Note ( Rhythm, Mode, KeyName, KeySig, TimeSig, AbcContext (..), 
                  beats, genScale )
import Abc.Midi ( loadMidiTrack, midiToChar )
import Codec.Midi

data TuneArgs = TuneArgs
  { trackno :: Int
  , intro   :: Dur
  , noteLen :: Dur
  , rhythm  :: Rhythm
  , key     :: KeyName
  , mode    :: Mode
  , name    :: String
  , input   :: String
  , output  :: String
  , quiet   :: Bool }

tuneArgs :: Parser TuneArgs
tuneArgs = TuneArgs
     <$> option auto
         ( long "trackno"
        <> short 't'
        <> metavar "<midi-track>"
        <> help "midi track number" )  
     <*> option auto
         ( long "leadin"
        <> short 'l'
        <> metavar "<lead-in-length>"
        <> help "lead-in bar length" ) 
     <*> option auto
         ( long "defaultnotelen"
        <> short 'd'
        <> metavar "<default-note-length>"
        <> help "default note length" )
     <*> option auto
         ( long "rhythm"
        <> short 'r'
        <> metavar "<rhythm>"
        <> help "Rhythm: Jig,Polska,Waltz.." )
     <*> option auto
         ( long "key"
        <> short 'k'
        <> metavar "<key>"
        <> help "Key: C,C#,D.." )
     <*> option auto
         ( long "mode"
        <> short 'm'
        <> metavar "<mode>"
        <> help "Mode: Major or Minor" )
     <*> strOption
         ( long "name"
        <> short 'n'
        <> metavar "<name>"
        <> help "Tune name" )
     <*> strOption
         ( long "input"
        <> short 'i'
        <> metavar "<input>"
        <> help "Input file" )
     <*> strOption
         ( long "output"
        <> short 'o'
        <> metavar "<output>"
        <> help "Output file" )
     <*> switch
         ( long "quiet"
        <> help "Whether to be quiet" )

        
tuneopts :: TuneArgs -> IO ()
tuneopts (TuneArgs t l d r k m n i o False) = 
         do
           -- initial context has an arbitrary Time Signature
           let ctx = AbcContext {ctxTrackNo = t,
                                 ctxName = n,
                                 ctxRhythm = r,
                                 ctxKeyName = k,
                                 ctxMode = m,
                                 ctxScale = genScale (k,m),
                                 ctxLeadIn = l,
                                 ctxTimeSig = (4,4),
                                 ctxDefaultNoteLen = d,
                                 ctxBeats = beats (4,4) }

           putStrLn $ "track no " ++ (show t) ++ " lead-in length " ++ (show l) ++ " default note length " ++ (show d) 
                       ++ " rhythm " ++ (show r) ++ " key "  ++ (show k) ++ " mode " ++ (show m) 
                       ++ " name " ++ n ++ " input " ++ i ++ " output " ++ o

           (f, ts) <- loadMidiTrack t i     

           -- update the context with the time signature from the midi
           let ctx1 = ctx { ctxTimeSig = ts, ctxBeats = beats ts }     

           writeFile o $ midiToChar f ctx1

           putStrLn $ "output written to " ++ o
    

tuneopts _ = return ()

main :: IO ()
main = execParser opts >>= tuneopts
  where
    opts = info (helper <*> tuneArgs)
      ( fullDesc
     <> progDesc "Get the tune parameters"
     <> header "trackno - the midi track number (always 0 if a single track file)"
     <> header "leadin - the lead-in bar length (e.g. 0/16 or 3/16)"
     <> header "defaultnotelen - the default note length (e.g. 1/8 or 1/16)"
     <> header "rhythm - [Jig,Polska,Waltz..]" 
     <> header "key - [C|C#|D..]" 
     <> header "mode - [Major|Minor]" 
     <> header "tune name"  
     <> header "input file path"  
     <> header "output file path" )
