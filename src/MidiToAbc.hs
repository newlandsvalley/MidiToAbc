import Options.Applicative
import Euterpea ( Dur )
import Abc.Note ( Rhythm, Mode, KeyName, KeySig, TimeSig, AbcContext (..), 
                  beats, genScale )
import Abc.Midi ( loadMidiFile, midiToChar )
import Codec.Midi

data TuneArgs = TuneArgs
  { intro   :: Dur
  , noteLen :: Dur
  , timesig :: TimeSig
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
         ( long "timesig"
        <> short 't'
        <> metavar "<time-signature>"
        <> help "Time signature" )
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
tuneopts (TuneArgs l d t r k m n i o False) = 
         do
           let ctx = AbcContext {ctxName = n,
                                 ctxRhythm = r,
                                 ctxKeyName = k,
                                 ctxMode = m,
                                 ctxScale = genScale (k,m),
                                 ctxLeadIn = l,
                                 ctxTimeSig = t,
                                 ctxDefaultNoteLen = d,
                                 ctxBeats = beats t }

           putStrLn $ "lead-in length " ++ (show l) ++ " default note length " ++ (show d) 
                  ++ " time signature "  ++ (show t) ++ " rhythm " ++ (show r) ++ " key " 
                  ++ (show k) ++ " mode " ++ (show m) ++ " name " ++ n ++ " input " ++ i ++ " output " ++ o

           f <- loadMidiFile i     

           writeFile o $ midiToChar f ctx

           putStrLn $ "output written to " ++ o
    

tuneopts _ = return ()

main :: IO ()
main = execParser opts >>= tuneopts
  where
    opts = info (helper <*> tuneArgs)
      ( fullDesc
     <> progDesc "Get the tune parameters"
     <> header "leadin - the lead-in bar length (e.g. 0/16 or 3/16)"
     <> header "defaultnotelen - the default note length (e.g. 1/8 or 1/16)"
     <> header "timesig - the time signature (e.g. (4,4)" 
     <> header "rhythm - [Jig,Polska,Waltz..]" 
     <> header "key - [C|C#|D..]" 
     <> header "mode - [Major|Minor]" 
     <> header "tune name"  
     <> header "input file path"  
     <> header "output file path" )
