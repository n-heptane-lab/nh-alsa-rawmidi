module Main where

import Control.Monad.Trans     (lift)
import Pipes                   ((>~), (>->), runEffect)
import qualified Pipes.Prelude as P
import Sound.NH.ALSA.RawMidi   as RawMidi (RawMode(..), openInput, read, strError)
import Sound.NH.MIDI.Parse     (parseMidi)
import System.Environment      (getArgs)
import System.Exit             (exitFailure)

main :: IO ()
main =
  do args <- getArgs
     case args of
       [midiDevice] ->
         do mh <- openInput midiDevice None
            case mh of
              (Left e) ->
                do putStrLn =<< strError e
                   exitFailure
              (Right h) ->
                do putStrLn $ "opened " ++ midiDevice ++ " for MIDI input."
                   runEffect $ (lift $ RawMidi.read h) >~ parseMidi >-> P.print
       _ -> do putStrLn "usage: dump-midi DEVICE\n  where DEVICE is an ALSA device name such as hw:2,0,0"
