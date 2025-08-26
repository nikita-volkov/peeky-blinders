module PtrPeeker.Variable where

import Data.ByteString.Char8 qualified as Bsc
import Data.ByteString.Internal qualified as Bsi
import Data.Vector.Generic qualified as Vg
import Data.Vector.Generic.Mutable qualified as Vgm
import Ptr.IO qualified
import PtrPeeker.Fixed
import PtrPeeker.Prelude

-- * Execution

-- |
-- Execute a variable decoder on a ByteString.
--
-- Returns either:
--
-- * The number of additional bytes required if input is too short
--
-- * Successfully decoded value
{-# INLINE runVariableOnByteString #-}
runVariableOnByteString :: Variable a -> ByteString -> Either Int a
runVariableOnByteString (Variable peek) (Bsi.PS bsFp bsOff bsSize) =
  unsafeDupablePerformIO
    $ withForeignPtr bsFp
    $ \p ->
      peek (return . Left) (\r _ _ -> return (Right r)) (plusPtr p bsOff) bsSize

-- |
-- Execute a variable decoder on a ByteString, returning both the decoded value and remaining bytes.
--
-- Returns either:
--
-- * The number of additional bytes required if input is too short
--
-- * Successfully decoded value and unconsumed bytes
runVariableOnByteStringWithRemainders :: Variable a -> ByteString -> Either Int (a, ByteString)
runVariableOnByteStringWithRemainders (Variable peek) (Bsi.PS bsFp bsOff bsSize) =
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
-- Execute a variable decoder on a pointer and an amount of available bytes in it.
--
-- Fails with the amount of extra bytes required at least if it\'s too short.
--
-- Succeeds returning the output, the next pointer, and the remaining available bytes.
{-# INLINE runVariableOnPtrWithRemainders #-}
runVariableOnPtrWithRemainders :: Variable a -> Ptr Word8 -> Int -> IO (Either Int (a, Ptr Word8, Int))
runVariableOnPtrWithRemainders (Variable peek) ptr avail =
  peek
    (pure . Left)
    (\output ptr avail -> return (Right (output, ptr, avail)))
    ptr
    avail

-- * Declarations

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
-- You can convert a 'Fixed' decoder to 'Variable' using 'fixed', but not
-- the other way around.
--
-- Example usage:
--
-- @
-- -- Decode a length-prefixed string
-- variableLengthString :: Variable ByteString
-- variableLengthString = do
--   len <- fixed beUnsignedInt4
--   fixed (byteArrayAsByteString (fromIntegral len))
-- @
newtype Variable output
  = Variable
      ( forall x.
        (Int -> IO x) ->
        (output -> Ptr Word8 -> Int -> IO x) ->
        (Ptr Word8 -> Int -> IO x)
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
-- Constrain a decoder to consume exactly the specified number of bytes.
--
-- Advances the position by the given amount regardless of how many bytes
-- the inner decoder actually consumes.
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
-- Lift a fixed decoder into the variable decoder context.
--
-- This allows you to use fixed-size decoders within variable-size parsing
-- contexts. The reverse conversion (Variable to Fixed) is not possible.
--
-- When decoding several fixed-size fields in sequence, prefer combining them
-- inside a single Fixed decoder and lifting once, instead of lifting each field
-- separately. This reduces overhead (fewer checks, fewer continuations, fewer
-- pointer adjustments).
--
-- === Example (comparing styles):
--
-- Less optimal: 3 separate lifts (3 bounds checks, 3 continuations)
--
-- >triple1 :: Variable (Word32, Word32, Word16)
-- >triple1 =
-- >  (,,)
-- >    <$> fixed beUnsignedInt4
-- >    <*> fixed beUnsignedInt4
-- >    <*> fixed beUnsignedInt2
--
-- More optimal: compose in Fixed, lift once (1 bounds check, 1 continuation)
--
-- >triple2 :: Variable (Word32, Word32, Word16)
-- >triple2 =
-- >  fixed
-- >    ( (,,)
-- >        <$> beUnsignedInt4
-- >        <*> beUnsignedInt4
-- >        <*> beUnsignedInt2
-- >    )
--
-- The second form is typically faster in hot code paths.
{-# INLINE fixed #-}
fixed :: Fixed a -> Variable a
fixed (Fixed size io) = Variable $ \fail proceed p avail ->
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

-- |
-- Consume all remaining bytes as a ByteString.
--
-- This decoder reads all available bytes from the current position to the end
-- of the input, returning them as a strict ByteString. After execution,
-- no bytes will remain available for subsequent decoders.
{-# INLINE remainderAsByteString #-}
remainderAsByteString :: Variable ByteString
remainderAsByteString = Variable $ \_ proceed p avail ->
  Ptr.IO.peekBytes p avail >>= \x -> proceed x (plusPtr p avail) 0
