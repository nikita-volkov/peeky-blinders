module PeekyBlinders
  ( decodeByteString,
    Dynamic,
    dynamize,
    Static,
    int32InBe,
    int32InLe,
    byteStringBySize,
  )
where

import PeekyBlinders.Prelude hiding (Dynamic)
import qualified Data.ByteString.Internal as ByteString
import qualified Ptr.IO

-- *

{-|
Execute a dynamic decoder on a bytestring,
failing with the amount of extra bytes required at least if it\'s too short.
-}
decodeByteString :: Dynamic a -> ByteString -> Either Int a
decodeByteString (Dynamic peek) (ByteString.PS bsFp bsOff bsSize) =
  unsafeDupablePerformIO $ withForeignPtr bsFp $ \p ->
    peek (return . Left) (\r _ _ -> return (Right r)) (plusPtr p bsOff) bsSize

-- *

{-|
Instruction on how to decode a data-structure of size only known at runtime.
-}
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

{-|
Convert a static decoder to the dynamic one.
You can\'t go the other way around.
-}
{-# INLINE dynamize #-}
dynamize :: Static a -> Dynamic a
dynamize (Static size io) = Dynamic $ \fail proceed p avail ->
  if avail >= size
    then io p >>= \x -> proceed x (plusPtr p size) (avail - size)
    else fail $ avail - size

-- *

{-|
Instruction on how to decode a data-structure of a statically known size.

Prefer composing on the level of this abstraction when possible,
since it\'s faster.
There is no way to lift a dynamic decoder to this level though,
so you can only compose out of static decoders here.
-}
data Static output =
  Static {-# UNPACK #-} !Int (Ptr Word8 -> IO output)

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

{-|
4-byte signed Big-Endian integer.
-}
{-# INLINE int32InBe #-}
int32InBe :: Static Int32
int32InBe = Static 4 Ptr.IO.peekBEInt32

{-|
4-byte signed Little-Endian integer.
-}
{-# INLINE int32InLe #-}
int32InLe :: Static Int32
int32InLe = Static 4 Ptr.IO.peekLEInt32

{-|
Collect a strict bytestring knowing its size.
-}
{-# INLINE byteStringBySize #-}
byteStringBySize :: Int -> Static ByteString
byteStringBySize size = Static size $ \p -> Ptr.IO.peekBytes p size
