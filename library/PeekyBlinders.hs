module PeekyBlinders
  ( -- * Execution
    decodeByteString,

    -- * Dynamic
    Dynamic,
    statically,
    nullTerminatedStringAsByteString,
    nullTerminatedStringAsShortByteString,

    -- * Static
    Static,
    beSignedInt4,
    leSignedInt4,
    byteArrayAsByteString,
    byteArrayAsShortByteString,
  )
where

import qualified Data.ByteString.Char8 as Bsc
import qualified Data.ByteString.Internal as ByteString
import PeekyBlinders.Prelude hiding (Dynamic)
import qualified Ptr.IO

-- *

-- |
-- Execute a dynamic decoder on a bytestring,
-- failing with the amount of extra bytes required at least if it\'s too short.
decodeByteString :: Dynamic a -> ByteString -> Either Int a
decodeByteString (Dynamic peek) (ByteString.PS bsFp bsOff bsSize) =
  unsafeDupablePerformIO $
    withForeignPtr bsFp $ \p ->
      peek (return . Left) (\r _ _ -> return (Right r)) (plusPtr p bsOff) bsSize

-- *

-- |
-- Instruction on how to decode a data-structure of size only known at runtime.
--
-- Provides for monadic composition,
-- where the output of one decoder determines what the following decoder should be.
--
-- Not all encodings require that much compositional freedom and
-- can be composed with a more restricted 'Static' decoder,
-- which provides for higher performance at the cost of a more restrictive
-- applicative composition.
newtype Dynamic output = Dynamic (forall x. (Int -> IO x) -> (output -> Ptr Word8 -> Int -> IO x) -> Ptr Word8 -> Int -> IO x)

instance Functor Dynamic where
  fmap f (Dynamic peek) = Dynamic $ \fail proceed -> peek fail (proceed . f)

instance Applicative Dynamic where
  pure a = Dynamic $ \_ proceed -> proceed a
  liftA2 f (Dynamic lPeek) (Dynamic rPeek) = Dynamic $ \fail proceed ->
    lPeek fail $ \lr -> rPeek fail $ \rr -> proceed (f lr rr)

instance Monad Dynamic where
  return = pure
  Dynamic lPeek >>= rk = Dynamic $ \fail proceed ->
    lPeek fail $ \lr -> case rk lr of Dynamic rPeek -> rPeek fail proceed

-- *

-- |
-- Convert a static decoder to the dynamic one.
-- You can\'t go the other way around.
{-# INLINE statically #-}
statically :: Static a -> Dynamic a
statically (Static size io) = Dynamic $ \fail proceed p avail ->
  if avail >= size
    then io p >>= \x -> proceed x (plusPtr p size) (avail - size)
    else fail $ size - avail

-- |
-- C-style string, which is a collection of bytes terminated by the first 0-valued byte.
-- This last byte is not included in the decoded value.
nullTerminatedStringAsByteString :: Dynamic ByteString
nullTerminatedStringAsByteString = Dynamic $ \fail proceed p avail -> do
  !bs <- Bsc.packCString (castPtr p)
  let sizeWithNull = succ (Bsc.length bs)
   in if avail < sizeWithNull
        then fail $ sizeWithNull - avail
        else proceed bs (plusPtr p sizeWithNull) (avail - sizeWithNull)

-- |
-- C-style string, which is a collection of bytes terminated by the first 0-valued byte.
-- This last byte is not included in the decoded value.
{-# INLINE nullTerminatedStringAsShortByteString #-}
nullTerminatedStringAsShortByteString :: Dynamic ShortByteString
nullTerminatedStringAsShortByteString = Dynamic $ \fail proceed p avail ->
  Ptr.IO.peekNullTerminatedShortByteString p $ \size build ->
    let sizeWithNull = succ size
     in if avail < sizeWithNull
          then fail $ sizeWithNull - avail
          else build >>= \x -> proceed x (plusPtr p sizeWithNull) (avail - sizeWithNull)

-- *

-- |
-- Instruction on how to decode a data-structure of a statically known size.
--
-- Prefer composing on the level of this abstraction when possible,
-- since it\'s faster.
-- There is no way to lift a dynamic decoder to this level though,
-- so you can only compose out of static decoders here.
data Static output
  = Static {-# UNPACK #-} !Int (Ptr Word8 -> IO output)

instance Functor Static where
  fmap fn (Static size io) = Static size (fmap fn . io)

instance Applicative Static where
  pure x = Static 0 (const (pure x))
  (<*>) (Static leftSize leftIO) (Static rightSize rightIO) =
    Static size io
    where
      size = leftSize + rightSize
      io ptr = leftIO ptr <*> rightIO (plusPtr ptr leftSize)

-- *

-- |
-- 4-byte signed Big-Endian integer.
{-# INLINE beSignedInt4 #-}
beSignedInt4 :: Static Int32
beSignedInt4 = Static 4 Ptr.IO.peekBEInt32

-- |
-- 4-byte signed Little-Endian integer.
{-# INLINE leSignedInt4 #-}
leSignedInt4 :: Static Int32
leSignedInt4 = Static 4 Ptr.IO.peekLEInt32

-- |
-- Collect a strict bytestring knowing its size.
--
-- Typically, you\'ll be using it like this:
--
-- @
-- byteString :: 'Dynamic' ByteString
-- byteString = 'statically' 'beSignedInt4' >>= 'statically' . 'byteArrayAsByteString' . fromIntegral
-- @
{-# INLINE byteArrayAsByteString #-}
byteArrayAsByteString :: Int -> Static ByteString
byteArrayAsByteString size = Static size $ \p -> Ptr.IO.peekBytes p size

-- |
-- Collect a short bytestring knowing its size.
--
-- Typically, you\'ll be using it like this:
--
-- @
-- shortByteString :: 'Dynamic' ShortByteString
-- shortByteString = 'statically' 'beSignedInt4' >>= 'statically' . 'byteArrayAsShortByteString' . fromIntegral
-- @
{-# INLINE byteArrayAsShortByteString #-}
byteArrayAsShortByteString :: Int -> Static ShortByteString
byteArrayAsShortByteString size = Static size $ \p -> Ptr.IO.peekShortByteString p size
