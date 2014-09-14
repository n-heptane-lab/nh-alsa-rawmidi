{- | binding to the ALSA rawmidi interface -}
module Sound.NH.ALSA.RawMidi where

#include <alsa/asoundlib.h>

import Control.Monad             (when)
import Data.Int                  (Int64)
import Data.Word                 (Word8, Word64)
import Foreign.Ptr               (Ptr, nullPtr)
import qualified Foreign.C.Types as C
import           Foreign.C.Types (CInt, CSize)
import Foreign.C.String          (CString, withCAString, peekCString, castCCharToChar)
import Foreign.Marshal.Alloc     (alloca, allocaBytes)
import Foreign.Storable          (peek)

data RawMidi = RawMidi

data RawMode
  = None
  | Append
  | NonBlock
  | Sync
    deriving (Eq, Ord, Read, Show)

rawModeConst :: RawMode -> C.CInt
rawModeConst None     = 0
rawModeConst Append   = #{const SND_RAWMIDI_APPEND}
rawModeConst NonBlock = #{const SND_RAWMIDI_NONBLOCK}
rawModeConst Sync     = #{const SND_RAWMIDI_SYNC}

foreign import ccall safe "alsa/asoundlib.h snd_rawmidi_open"
  snd_rawmidi_open :: Ptr (Ptr RawMidi) -> Ptr (Ptr RawMidi) -> CString -> CInt -> IO CInt

openInput :: String -> RawMode -> IO (Either CInt (Ptr RawMidi))
openInput name mode =
  alloca $ \phandle_in ->
    withCAString name $ \cname ->
     do r <- snd_rawmidi_open phandle_in nullPtr cname (rawModeConst mode)
        if r == 0
          then do handle_in  <- peek phandle_in
                  return (Right handle_in)
          else return (Left r)


foreign import ccall safe "alsa/asoundlib.h snd_rawmidi_read"
  snd_rawmidi_read :: Ptr RawMidi -> Ptr a -> (#type size_t) -> IO (#type ssize_t)

read :: Ptr RawMidi -> IO Word8
read handle_in =
  allocaBytes 1 $ \pword8 ->
   do r <- snd_rawmidi_read handle_in pword8 1
      when (r < 0) (do msg <- strError r ; error msg)
      c <- peek pword8
      return c

foreign import ccall unsafe "alsa/asoundlib.h snd_strerror"
  snd_strerror :: CInt -> CString

strError :: (Integral i) => i -> IO String
strError i =
  peekCString (snd_strerror  (fromIntegral i))

