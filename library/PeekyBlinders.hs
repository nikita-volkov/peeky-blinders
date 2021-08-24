module PeekyBlinders
  ( -- * Execution
    decodeByteString,

    -- * Dynamic
    Dynamic,
    statically,
    nullTerminatedStringAsByteString,
    nullTerminatedStringAsShortByteString,
    dynamicArray,

    -- * Static
    Static,

    -- ** Unsigned Integers
    unsignedInt1,
    beUnsignedInt2,
    leUnsignedInt2,
    beUnsignedInt4,
    leUnsignedInt4,
    beUnsignedInt8,
    leUnsignedInt8,

    -- ** Signed Integers
    signedInt1,
    beSignedInt2,
    leSignedInt2,
    beSignedInt4,
    leSignedInt4,
    beSignedInt8,
    leSignedInt8,

    -- ** Arrays
    byteArrayAsByteString,
    byteArrayAsShortByteString,
    staticArray,
  )
where

import qualified Data.ByteString.Char8 as Bsc
import qualified Data.ByteString.Internal as ByteString
import qualified Data.Vector.Generic as Vg
import qualified Data.Vector.Generic.Mutable as Vgm
import qualified Data.Vector.Unboxed as Vu
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

-- |
-- Array of dynamically sized elements of the specified amount.
{-# INLINE dynamicArray #-}
dynamicArray :: Vg.Vector v a => Dynamic a -> Int -> Dynamic (v a)
dynamicArray (Dynamic peekElement) amount = Dynamic $ \fail proceed p avail -> do
  v <- Vgm.unsafeNew amount
  let populate i p avail =
        if i < amount
          then peekElement fail (\a p avail -> Vgm.unsafeWrite v i a >> populate (succ i) p avail) p avail
          else Vg.unsafeFreeze v >>= \v -> proceed v p avail
   in populate 0 p avail

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
-- 1-byte unsigned integer.
{-# INLINE unsignedInt1 #-}
unsignedInt1 :: Static Word8
unsignedInt1 = Static 1 Ptr.IO.peekWord8

-- |
-- 2-byte unsigned Big-Endian integer.
{-# INLINE beUnsignedInt2 #-}
beUnsignedInt2 :: Static Word16
beUnsignedInt2 = Static 2 Ptr.IO.peekBEWord16

-- |
-- 2-byte unsigned Little-Endian integer.
{-# INLINE leUnsignedInt2 #-}
leUnsignedInt2 :: Static Word16
leUnsignedInt2 = Static 2 Ptr.IO.peekLEWord16

-- |
-- 4-byte unsigned Big-Endian integer.
{-# INLINE beUnsignedInt4 #-}
beUnsignedInt4 :: Static Word32
beUnsignedInt4 = Static 4 Ptr.IO.peekBEWord32

-- |
-- 4-byte unsigned Little-Endian integer.
{-# INLINE leUnsignedInt4 #-}
leUnsignedInt4 :: Static Word32
leUnsignedInt4 = Static 4 Ptr.IO.peekLEWord32

-- |
-- 8-byte unsigned Big-Endian integer.
{-# INLINE beUnsignedInt8 #-}
beUnsignedInt8 :: Static Word64
beUnsignedInt8 = Static 8 Ptr.IO.peekBEWord64

-- |
-- 8-byte unsigned Little-Endian integer.
{-# INLINE leUnsignedInt8 #-}
leUnsignedInt8 :: Static Word64
leUnsignedInt8 = Static 8 Ptr.IO.peekLEWord64

-- |
-- 1-byte signed integer.
{-# INLINE signedInt1 #-}
signedInt1 :: Static Int8
signedInt1 = Static 1 Ptr.IO.peekInt8

-- |
-- 2-byte signed Big-Endian integer.
{-# INLINE beSignedInt2 #-}
beSignedInt2 :: Static Int16
beSignedInt2 = Static 2 Ptr.IO.peekBEInt16

-- |
-- 2-byte signed Little-Endian integer.
{-# INLINE leSignedInt2 #-}
leSignedInt2 :: Static Int16
leSignedInt2 = Static 2 Ptr.IO.peekLEInt16

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
-- 8-byte signed Big-Endian integer.
{-# INLINE beSignedInt8 #-}
beSignedInt8 :: Static Int64
beSignedInt8 = Static 8 Ptr.IO.peekBEInt64

-- |
-- 8-byte signed Little-Endian integer.
{-# INLINE leSignedInt8 #-}
leSignedInt8 :: Static Int64
leSignedInt8 = Static 8 Ptr.IO.peekLEInt64

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

{-# INLINE byteArrayAsVector #-}
byteArrayAsVector :: Int -> Static (Vu.Vector Word8)
byteArrayAsVector = error "TODO"

-- |
-- Construct an array of the specified amount of statically sized elements.
{-# INLINE staticArray #-}
staticArray :: Vg.Vector v a => Static a -> Int -> Static (v a)
staticArray (Static elementSize peekElement) amount =
  Static (elementSize * amount) $ \p -> do
    v <- Vgm.unsafeNew amount
    let populate i p =
          if i < amount
            then do
              a <- peekElement p
              Vgm.unsafeWrite v i a
              populate (succ i) (plusPtr p elementSize)
            else Vg.unsafeFreeze v
     in populate 0 p
