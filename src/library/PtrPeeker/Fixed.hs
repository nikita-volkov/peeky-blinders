module PtrPeeker.Fixed where

import Data.ByteString.Internal qualified as Bsi
import Data.Vector.Generic qualified as Vg
import Data.Vector.Generic.Mutable qualified as Vgm
import Ptr.IO qualified
import PtrPeeker.Prelude

-- * Execution

-- |
-- Execute a fixed decoder on a ByteString.
--
-- Returns either:
--
-- * The number of additional bytes required if input is too short
--
-- * Successfully decoded value
{-# INLINE decodeByteStringWithFixed #-}
decodeByteStringWithFixed :: Fixed a -> ByteString -> Either Int a
decodeByteStringWithFixed (Fixed size peek) (Bsi.PS bsFp bsOff bsSize) =
  if bsSize > size
    then Right . unsafeDupablePerformIO . withForeignPtr bsFp $ \p ->
      peek (plusPtr p bsOff)
    else Left $ size - bsSize

-- * Declarations

-- |
-- A highly optimized decoder for fixed-size data structures.
--
-- 'Fixed' decoders are the most efficient way to decode binary data when
-- the size of each element is known at compile time. They provide excellent
-- performance characteristics due to their predictable memory access patterns
-- and lack of runtime size calculations.
--
-- Use 'Fixed' when:
--
-- * Decoding primitive types (integers, floats)
-- * Working with fixed-size records or structures
-- * Performance is critical and data layout is predictable
-- * The binary format has a static, well-defined structure
--
-- 'Fixed' decoders can be lifted to 'Variable' using 'fixed', but the
-- reverse conversion is not possible due to the compile-time size requirement.
--
-- Example usage:
--
-- >-- Decode a fixed-size record
-- >data Point = Point Int32 Int32 Int32
-- >
-- >point :: Fixed Point
-- >point = Point <$> beSignedInt4 <*> beSignedInt4 <*> beSignedInt4
data Fixed output
  = -- |
    -- The 'Fixed' constructor takes two parameters:
    --
    -- * Size in bytes (must be exact)
    -- * IO action to perform the actual decoding from a pointer
    Fixed {-# UNPACK #-} !Int (Ptr Word8 -> IO output)

instance Functor Fixed where
  fmap fn (Fixed size io) = Fixed size (fmap fn . io)

instance Applicative Fixed where
  pure x = Fixed 0 (const (pure x))
  (<*>) (Fixed leftSize leftIO) (Fixed rightSize rightIO) =
    Fixed size io
    where
      size = leftSize + rightSize
      io ptr = leftIO ptr <*> rightIO (plusPtr ptr leftSize)

-- |
-- Skip the specified number of bytes without consuming them.
--
-- This is useful for advancing past padding or unused fields in binary formats.
-- The decoder succeeds immediately without reading any data.
{-# INLINE skip #-}
skip :: Int -> Fixed ()
skip amount = Fixed amount (const (pure ()))

-- |
-- 1-byte unsigned integer.
{-# INLINE unsignedInt1 #-}
unsignedInt1 :: Fixed Word8
unsignedInt1 = Fixed 1 Ptr.IO.peekWord8

-- |
-- 2-byte unsigned Big-Endian integer.
{-# INLINE beUnsignedInt2 #-}
beUnsignedInt2 :: Fixed Word16
beUnsignedInt2 = Fixed 2 Ptr.IO.peekBEWord16

-- |
-- 2-byte unsigned Little-Endian integer.
{-# INLINE leUnsignedInt2 #-}
leUnsignedInt2 :: Fixed Word16
leUnsignedInt2 = Fixed 2 Ptr.IO.peekLEWord16

-- |
-- 4-byte unsigned Big-Endian integer.
{-# INLINE beUnsignedInt4 #-}
beUnsignedInt4 :: Fixed Word32
beUnsignedInt4 = Fixed 4 Ptr.IO.peekBEWord32

-- |
-- 4-byte unsigned Little-Endian integer.
{-# INLINE leUnsignedInt4 #-}
leUnsignedInt4 :: Fixed Word32
leUnsignedInt4 = Fixed 4 Ptr.IO.peekLEWord32

-- |
-- 8-byte unsigned Big-Endian integer.
{-# INLINE beUnsignedInt8 #-}
beUnsignedInt8 :: Fixed Word64
beUnsignedInt8 = Fixed 8 Ptr.IO.peekBEWord64

-- |
-- 8-byte unsigned Little-Endian integer.
{-# INLINE leUnsignedInt8 #-}
leUnsignedInt8 :: Fixed Word64
leUnsignedInt8 = Fixed 8 Ptr.IO.peekLEWord64

-- |
-- 1-byte signed integer.
{-# INLINE signedInt1 #-}
signedInt1 :: Fixed Int8
signedInt1 = Fixed 1 Ptr.IO.peekInt8

-- |
-- 2-byte signed Big-Endian integer.
{-# INLINE beSignedInt2 #-}
beSignedInt2 :: Fixed Int16
beSignedInt2 = Fixed 2 Ptr.IO.peekBEInt16

-- |
-- 2-byte signed Little-Endian integer.
{-# INLINE leSignedInt2 #-}
leSignedInt2 :: Fixed Int16
leSignedInt2 = Fixed 2 Ptr.IO.peekLEInt16

-- |
-- 4-byte signed Big-Endian integer.
{-# INLINE beSignedInt4 #-}
beSignedInt4 :: Fixed Int32
beSignedInt4 = Fixed 4 Ptr.IO.peekBEInt32

-- |
-- 4-byte signed Little-Endian integer.
{-# INLINE leSignedInt4 #-}
leSignedInt4 :: Fixed Int32
leSignedInt4 = Fixed 4 Ptr.IO.peekLEInt32

-- |
-- 8-byte signed Big-Endian integer.
{-# INLINE beSignedInt8 #-}
beSignedInt8 :: Fixed Int64
beSignedInt8 = Fixed 8 Ptr.IO.peekBEInt64

-- |
-- 8-byte signed Little-Endian integer.
{-# INLINE leSignedInt8 #-}
leSignedInt8 :: Fixed Int64
leSignedInt8 = Fixed 8 Ptr.IO.peekLEInt64

-- |
-- Collect a strict bytestring knowing its size.
--
-- Typically, you\'ll be using it like this:
--
-- @
-- byteString :: 'Variable' ByteString
-- byteString = 'fixed' 'beSignedInt4' >>= 'fixed' . 'byteArrayAsByteString' . fromIntegral
-- @
{-# INLINE byteArrayAsByteString #-}
byteArrayAsByteString :: Int -> Fixed ByteString
byteArrayAsByteString size = Fixed size $ \p -> Ptr.IO.peekBytes p size

-- |
-- Collect a short bytestring knowing its size.
--
-- Typically, you\'ll be using it like this:
--
-- @
-- shortByteString :: 'Variable' ShortByteString
-- shortByteString = 'fixed' 'beSignedInt4' >>= 'fixed' . 'byteArrayAsShortByteString' . fromIntegral
-- @
{-# INLINE byteArrayAsShortByteString #-}
byteArrayAsShortByteString :: Int -> Fixed ShortByteString
byteArrayAsShortByteString size = Fixed size $ \p -> Ptr.IO.peekShortByteString p size

-- |
-- Construct an array of the specified amount of fixed sized elements.
{-# INLINE fixedArray #-}
fixedArray :: (Vg.Vector v a) => Fixed a -> Int -> Fixed (v a)
fixedArray (Fixed elementSize peekElement) amount =
  Fixed (elementSize * amount) $ \p -> do
    v <- Vgm.unsafeNew amount
    let populate i p =
          if i < amount
            then do
              a <- peekElement p
              Vgm.unsafeWrite v i a
              populate (succ i) (plusPtr p elementSize)
            else Vg.unsafeFreeze v
     in populate 0 p

-- | Fixed-size peeker for any Storable type.
{-# INLINE storable #-}
storable :: forall a. (Storable a) => Fixed a
storable = Fixed (sizeOf (undefined :: a)) Ptr.IO.peekStorable
