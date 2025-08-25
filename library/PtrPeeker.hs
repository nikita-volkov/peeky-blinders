module PtrPeeker
  ( -- * Execution
    decodeByteStringVariably,
    decodeByteStringVariablyWithRemainders,
    decodeByteStringFixedly,
    decodePtrVariablyWithRemainders,

    -- * Variable
    Variable,
    hasMore,
    forceSize,
    fixedly,
    nullTerminatedStringAsByteString,
    nullTerminatedStringAsShortByteString,
    variableArray,
    remainderAsByteString,

    -- * Fixed
    Fixed,
    skip,

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
    fixedArray,
  )
where

import Data.ByteString.Char8 qualified as Bsc
import Data.ByteString.Internal qualified as Bsi
import Data.Vector.Generic qualified as Vg
import Data.Vector.Generic.Mutable qualified as Vgm
import Ptr.IO qualified
import PtrPeeker.Prelude

-- * Execution

-- |
-- Execute a variable decoder on a bytestring,
-- failing with the amount of extra bytes required at least if it\'s too short.
{-# INLINE decodeByteStringVariably #-}
decodeByteStringVariably :: Variable a -> ByteString -> Either Int a
decodeByteStringVariably (Variable peek) (Bsi.PS bsFp bsOff bsSize) =
  unsafeDupablePerformIO
    $ withForeignPtr bsFp
    $ \p ->
      peek (return . Left) (\r _ _ -> return (Right r)) (plusPtr p bsOff) bsSize

-- |
-- Execute a variable decoder on a bytestring returning both the decoded value and remaining bytes.
--
-- This function takes a 'Variable' decoder and a 'ByteString', attempts to decode a value of type 'a'
-- from the beginning of the ByteString, and returns either:
--
-- * 'Left Int' - Error indicating the amount of extra bytes required at least if the ByteString is too short to decode a value of type 'a'.
-- * 'Right (a, ByteString)' - Successfully decoded value along with the remaining unconsumed bytes.
decodeByteStringVariablyWithRemainders :: Variable a -> ByteString -> Either Int (a, ByteString)
decodeByteStringVariablyWithRemainders (Variable peek) (Bsi.PS bsFp bsOff bsSize) =
  unsafeDupablePerformIO
    $ withForeignPtr bsFp
    $ \ptr ->
      let initialPtr = plusPtr ptr bsOff
       in peek
            (return . Left)
            ( \output newPtr avail ->
                return
                  ( Right
                      ( output,
                        Bsi.PS bsFp (bsOff + minusPtr newPtr initialPtr) avail
                      )
                  )
            )
            initialPtr
            bsSize

-- |
-- Execute a fixed decoder on a bytestring,
-- failing with the amount of extra bytes required at least if it\'s too short.
{-# INLINE decodeByteStringFixedly #-}
decodeByteStringFixedly :: Fixed a -> ByteString -> Either Int a
decodeByteStringFixedly (Fixed size peek) (Bsi.PS bsFp bsOff bsSize) =
  if bsSize > size
    then Right . unsafeDupablePerformIO . withForeignPtr bsFp $ \p ->
      peek (plusPtr p bsOff)
    else Left $ size - bsSize

-- |
-- Execute a variable decoder on a pointer and an amount of available bytes in it.
--
-- Fails with the amount of extra bytes required at least if it\'s too short.
--
-- Succeeds returning the output, the next pointer, and the remaining available bytes.
{-# INLINE decodePtrVariablyWithRemainders #-}
decodePtrVariablyWithRemainders :: Variable a -> Ptr Word8 -> Int -> IO (Either Int (a, Ptr Word8, Int))
decodePtrVariablyWithRemainders (Variable peek) ptr avail =
  peek
    (pure . Left)
    (\output ptr avail -> return (Right (output, ptr, avail)))
    ptr
    avail

-- * Variable

-- |
-- A high-level decoder for variable-sized data structures where the size
-- is only known at runtime.
--
-- 'Variable' decoders are optimized for processing both multi-chunk and
-- single-chunk input efficiently. They provide full monadic composition,
-- allowing the output of one decoder to determine what the following
-- decoder should be. This makes them ideal for complex binary formats
-- where the structure depends on previously decoded values.
--
-- Use 'Variable' when:
--
-- * Decoding length-prefixed data (arrays, strings)
-- * The structure depends on previously decoded values
-- * You need conditional or data-dependent parsing
-- * Working with formats that have variable-length encoding
--
-- For better performance with fixed-size data, consider using 'Fixed' instead.
-- You can convert a 'Fixed' decoder to 'Variable' using 'fixedly', but not
-- the other way around.
--
-- Example usage:
--
-- @
-- -- Decode a length-prefixed string
-- variableLengthString :: Variable ByteString
-- variableLengthString = do
--   len <- fixedly beUnsignedInt4
--   fixedly (byteArrayAsByteString (fromIntegral len))
-- @
newtype Variable output
  = Variable
      ( forall x.
        (Int -> IO x) ->
        (output -> Ptr Word8 -> Int -> IO x) ->
        Ptr Word8 ->
        Int ->
        IO x
      )

instance Functor Variable where
  {-# INLINE fmap #-}
  fmap f (Variable peek) = Variable $ \fail proceed -> peek fail (proceed . f)

instance Applicative Variable where
  pure a = Variable $ \_ proceed -> proceed a
  {-# INLINE (<*>) #-}
  Variable lPeek <*> Variable rPeek = Variable $ \fail proceed ->
    lPeek fail $ \lr -> rPeek fail $ \rr -> proceed (lr rr)
  {-# INLINE liftA2 #-}
  liftA2 f (Variable lPeek) (Variable rPeek) = Variable $ \fail proceed ->
    lPeek fail $ \lr -> rPeek fail $ \rr -> proceed (f lr rr)

instance Monad Variable where
  return = pure
  Variable lPeek >>= rk = Variable $ \fail proceed ->
    lPeek fail $ \lr -> case rk lr of Variable rPeek -> rPeek fail proceed

instance MonadIO Variable where
  liftIO io = Variable $ \_ proceed p avail -> io >>= \res -> proceed res p avail

-- |
-- Check whether more data is available.
hasMore :: Variable Bool
hasMore = Variable $ \_ proceed p avail -> proceed (avail > 0) p avail

-- |
-- Set an upper limit of available bytes to the specified amount for a decoder
-- and advance the same amount of bytes regardless of how many is actually consumed.
{-# INLINE forceSize #-}
forceSize :: Int -> Variable a -> Variable a
forceSize size (Variable dec) =
  Variable $ \fail proceed p avail ->
    if size > avail
      then fail (size - avail)
      else
        let nextPtr = plusPtr p size
            nextAvail = avail - size
            newProceed o _ _ = proceed o nextPtr nextAvail
         in dec fail newProceed p (min size avail)

-- |
-- Convert a fixed decoder to the variable one.
-- You can\'t go the other way around.
{-# INLINE fixedly #-}
fixedly :: Fixed a -> Variable a
fixedly (Fixed size io) = Variable $ \fail proceed p avail ->
  if avail >= size
    then io p >>= \x -> proceed x (plusPtr p size) (avail - size)
    else fail $ size - avail

-- |
-- C-style string, which is a collection of bytes terminated by the first 0-valued byte.
-- This last byte is not included in the decoded value.
{-# INLINE nullTerminatedStringAsByteString #-}
nullTerminatedStringAsByteString :: Variable ByteString
nullTerminatedStringAsByteString = Variable $ \fail proceed p avail -> do
  !bs <- Bsc.packCString (castPtr p)
  let sizeWithNull = succ (Bsc.length bs)
   in if avail < sizeWithNull
        then fail $ sizeWithNull - avail
        else proceed bs (plusPtr p sizeWithNull) (avail - sizeWithNull)

-- |
-- C-style string, which is a collection of bytes terminated by the first 0-valued byte.
-- This last byte is not included in the decoded value.
{-# INLINE nullTerminatedStringAsShortByteString #-}
nullTerminatedStringAsShortByteString :: Variable ShortByteString
nullTerminatedStringAsShortByteString = Variable $ \fail proceed p avail ->
  Ptr.IO.peekNullTerminatedShortByteString p $ \size build ->
    let sizeWithNull = succ size
     in if avail < sizeWithNull
          then fail $ sizeWithNull - avail
          else build >>= \x -> proceed x (plusPtr p sizeWithNull) (avail - sizeWithNull)

-- |
-- Array of variable sized elements of the specified amount.
{-# INLINE variableArray #-}
variableArray :: (Vg.Vector v a) => Variable a -> Int -> Variable (v a)
variableArray (Variable peekElement) amount = Variable $ \fail proceed p avail -> do
  v <- Vgm.unsafeNew amount
  let populate i p avail =
        if i < amount
          then peekElement fail (\a p avail -> Vgm.unsafeWrite v i a >> populate (succ i) p avail) p avail
          else Vg.unsafeFreeze v >>= \v -> proceed v p avail
   in populate 0 p avail

{-# INLINE remainderAsByteString #-}
remainderAsByteString :: Variable ByteString
remainderAsByteString = Variable $ \_ proceed p avail ->
  Ptr.IO.peekBytes p avail >>= \x -> proceed x (plusPtr p avail) 0

-- * Fixed

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
-- 'Fixed' decoders can be lifted to 'Variable' using 'fixedly', but the
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
-- byteString = 'fixedly' 'beSignedInt4' >>= 'fixedly' . 'byteArrayAsByteString' . fromIntegral
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
-- shortByteString = 'fixedly' 'beSignedInt4' >>= 'fixedly' . 'byteArrayAsShortByteString' . fromIntegral
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
