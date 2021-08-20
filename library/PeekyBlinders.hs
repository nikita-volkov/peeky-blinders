module PeekyBlinders
  ( decodeByteString,
    Dynamic,
    staticallySized,
    Static,
    int32InBe,
    int32InLe,
  )
where

import PeekyBlinders.Prelude hiding (Dynamic)
import qualified Data.ByteString.Internal as ByteString
import qualified Ptr.IO

{-|
Instruction on how to decode a data-structure of size only known at runtime.
-}
newtype Dynamic a = Dynamic (forall x. (Int -> IO x) -> (a -> Ptr Word8 -> Int -> IO x) -> Ptr Word8 -> Int -> IO x)

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

decodeByteString :: Dynamic a -> ByteString -> Either Int a
decodeByteString (Dynamic peek) (ByteString.PS bsFp bsOff bsSize) =
  unsafeDupablePerformIO $ withForeignPtr bsFp $ \p ->
    peek (return . Left) (\r _ _ -> return (Right r)) (plusPtr p bsOff) bsSize

-- *

{-# INLINE staticallySized #-}
staticallySized :: Static a -> Dynamic a
staticallySized (Static size io) = Dynamic $ \fail proceed p avail ->
  if avail >= size
    then io p >>= \x -> proceed x (plusPtr p size) (avail - size)
    else fail $ avail - size

-- *

{-|
Instruction on how to decode a data-structure of a statically known size.

Prefer composing on the level of this abstraction when possible,
since it\'s faster.
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

{-# INLINE int32InBe #-}
int32InBe :: Static Int32
int32InBe = Static 4 Ptr.IO.peekBEInt32

{-# INLINE int32InLe #-}
int32InLe :: Static Int32
int32InLe = Static 4 Ptr.IO.peekLEInt32
