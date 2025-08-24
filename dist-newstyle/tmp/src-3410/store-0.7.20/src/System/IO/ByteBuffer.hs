{-# LANGUAGE ScopedTypeVariables #-}
{-@ LIQUID "--no-termination" @-}
{-@ LIQUID "--short-names"    @-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-|
Module: System.IO.ByteBuffer
Description: Provides an efficient buffering abstraction.

A 'ByteBuffer' is a simple buffer for bytes.  It supports two
operations: refilling with the contents of a 'ByteString', and
consuming a fixed number of bytes.

It is implemented as a pointer, together with counters that keep track
of the offset and the number of bytes in the buffer.  Note that the
counters are simple 'IORef's, so 'ByteBuffer's are not thread-safe!

A 'ByteBuffer' is constructed by 'new' with a given starting length,
and will grow (by repeatedly multiplying its size by 1.5) whenever it
is being fed a 'ByteString' that is too large.
-}

module System.IO.ByteBuffer
       ( ByteBuffer
         -- * Allocation and Deallocation
       , new, free, with
         -- * Query for number of available bytes
       , totalSize, isEmpty, availableBytes
         -- * Feeding new input
       , copyByteString
#ifndef mingw32_HOST_OS
       , fillFromFd
#endif
         -- * Consuming bytes from the buffer
       , consume, unsafeConsume
         -- * Exceptions
       , ByteBufferException (..)
       ) where

import           Control.Applicative
import           Control.Exception (SomeException, throwIO)
import           Control.Exception.Lifted (Exception, bracket, catch)
import qualified Control.Monad.Fail as Fail
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BS
import           Data.IORef
import           Data.Maybe (fromMaybe)
import           Data.Typeable (Typeable)
import           Data.Word
import           Foreign.ForeignPtr
import qualified Foreign.Marshal.Alloc as Alloc
import           Foreign.Marshal.Utils (copyBytes, moveBytes)
import           GHC.Ptr
import           Prelude
import qualified Foreign.C.Error as CE
import           Foreign.C.Types
import           System.Posix.Types (Fd (..))

-- | A buffer into which bytes can be written.
--
-- Invariants:
--
-- * @size >= containedBytes >= consumedBytes >= 0@
--
-- * The range from @ptr@ to @ptr `plusPtr` size@ will be allocated
--
-- * The range from @ptr@ to @ptr `plusPtr` containedBytes@ will
--   contain bytes previously copied to the buffer
--
-- * The buffer contains @containedBytes - consumedBytes@ bytes of
--   data that have been copied to it, but not yet read.  They are in
--   the range from @ptr `plusPtr` consumedBytes@ to @ptr `plusPtr`
--   containedBytes@.
--
-- The first two invariants are encoded in Liquid Haskell, and can
-- be statically checked.
--
-- If an Exception occurs during an operation that modifies a
-- 'ByteBuffer', the 'ByteBuffer' is invalidated and can no longer be
-- used.  Trying to access the 'ByteBuffer' subsequently will result
-- in a 'ByteBufferException' being thrown.
{-@
data BBRef = BBRef
    { size :: {v: Int | v >= 0 }
    , contained :: { v: Int | v >= 0 && v <= size }
    , consumed :: { v: Int | v >= 0 && v <= contained }
    , ptr :: { v: Ptr Word8 | (plen v) = size }
    }
@-}

data BBRef = BBRef {
      size      :: {-# UNPACK #-} !Int
      -- ^ The amount of memory allocated.
    , contained :: {-# UNPACK #-} !Int
      -- ^ The number of bytes that the 'ByteBuffer' currently holds.
    , consumed  :: {-# UNPACK #-} !Int
      -- ^ The number of bytes that have already been consumed.
    , ptr       :: {-# UNPACK #-} !(Ptr Word8)
      -- ^ This points to the beginning of the memory allocated for
      -- the 'ByteBuffer'
    }

-- | Exception that is thrown when an invalid 'ByteBuffer' is being used that is no longer valid.
--
-- A 'ByteBuffer' is considered to be invalid if
--
-- - it has explicitly been freed
-- - an Exception has occured during an operation that modified it
data ByteBufferException = ByteBufferException
    { _bbeLocation :: !String
      -- ^ function name that caused the exception
    , _bbeException :: !String
      -- ^ printed representation of the exception
    }
    deriving (Typeable, Eq)
instance Show ByteBufferException where
    show (ByteBufferException loc e) = concat
        ["ByteBufferException: ByteBuffer was invalidated because of Exception thrown in "
        , loc , ": ", e]
instance Exception ByteBufferException

type ByteBuffer = IORef (Either ByteBufferException BBRef)

-- | On any Exception, this will invalidate the ByteBuffer and re-throw the Exception.
--
-- Invalidating the 'ByteBuffer' includes freeing the underlying pointer.
bbHandler :: MonadIO m
    => String
       -- ^ location information: function from which the exception was thrown
    -> ByteBuffer
       -- ^ this 'ByteBuffer' will be invalidated when an Exception occurs
    -> (BBRef -> IO a)
    -> m a
bbHandler loc bb f = liftIO $ useBBRef f bb `catch` \(e :: SomeException) -> do
    readIORef bb >>= \case
        Right bbref -> do
            Alloc.free (ptr bbref)
            writeIORef bb (Left $ ByteBufferException loc (show e))
        Left _ -> return ()
    throwIO e

-- | Try to use the 'BBRef' of a 'ByteBuffer', or throw a 'ByteBufferException' if it's invalid.
useBBRef :: (BBRef -> IO a) -> ByteBuffer -> IO a
useBBRef f bb = readIORef bb >>= either throwIO f
{-# INLINE useBBRef #-}

totalSize :: MonadIO m => ByteBuffer -> m Int
totalSize = liftIO . useBBRef (return . size)
{-# INLINE totalSize #-}

isEmpty :: MonadIO m => ByteBuffer -> m Bool
isEmpty bb = liftIO $ (==0) <$> availableBytes bb
{-# INLINE isEmpty #-}

-- | Number of available bytes in a 'ByteBuffer' (that is, bytes that
-- have been copied to, but not yet read from the 'ByteBuffer'.
{-@ availableBytes :: MonadIO m => ByteBuffer -> m {v: Int | v >= 0} @-}
availableBytes :: MonadIO m => ByteBuffer -> m Int
availableBytes = liftIO . useBBRef (\BBRef{..} -> return (contained - consumed))
{-# INLINE availableBytes #-}

-- | Allocates a new ByteBuffer with a given buffer size filling from
-- the given FillBuffer.
--
-- Note that 'ByteBuffer's created with 'new' have to be deallocated
-- explicitly using 'free'.  For automatic deallocation, consider
-- using 'with' instead.
new :: MonadIO m
    => Maybe Int
    -- ^ Size of buffer to allocate.  If 'Nothing', use the default
    -- value of 4MB
    -> m ByteBuffer
    -- ^ The byte buffer.
new ml = liftIO $ do
    let l = max 0 . fromMaybe (4*1024*1024) $ ml
    newPtr <- Alloc.mallocBytes l
    newIORef $ Right BBRef
        { ptr = newPtr
        , size = l
        , contained = 0
        , consumed = 0
        }

-- | Free a byte buffer.
free :: MonadIO m => ByteBuffer -> m ()
free bb = liftIO $ readIORef bb >>= \case
    Right bbref -> do
        Alloc.free $ ptr bbref
        writeIORef bb $
            Left (ByteBufferException "free" "ByteBuffer has explicitly been freed and is no longer valid.")
    Left _ -> return () -- the ByteBuffer is either invalid or has already been freed.

-- | Perform some action with a bytebuffer, with automatic allocation
-- and deallocation.
with :: (MonadIO m, MonadBaseControl IO m)
     => Maybe Int
     -- ^ Initial length of the 'ByteBuffer'.  If 'Nothing', use the
     -- default value of 4MB.
     -> (ByteBuffer -> m a)
     -> m a
with l action =
  bracket
    (new l)
    free
    action
{-# INLINE with #-}

-- | Reset a 'BBRef', i.e. copy all the bytes that have not yet
-- been consumed to the front of the buffer.
{-@ resetBBRef :: b:BBRef -> IO {v:BBRef | consumed v == 0 && contained v == contained b - consumed b && size v == size b} @-}
resetBBRef :: BBRef -> IO BBRef
resetBBRef bbref = do
    let available = contained bbref - consumed bbref
    moveBytes (ptr bbref) (ptr bbref `plusPtr` consumed bbref) available
    return BBRef { size = size bbref
                 , contained = available
                 , consumed = 0
                 , ptr = ptr bbref
                 }

-- | Make sure the buffer is at least @minSize@ bytes long.
--
-- In order to avoid having to enlarge the buffer too often, we
-- multiply its size by a factor of 1.5 until it is at least @minSize@
-- bytes long.
{-@ enlargeBBRef :: b:BBRef -> i:Nat -> IO {v:BBRef | size v >= i && contained v == contained b && consumed v == consumed b} @-}
enlargeBBRef :: BBRef -> Int -> IO BBRef
enlargeBBRef bbref minSize = do
        let getNewSize s | s >= minSize = s
            getNewSize s = getNewSize $ (ceiling . (*(1.5 :: Double)) . fromIntegral) (max 1 s)
            newSize = getNewSize (size bbref)
        -- possible optimisation: since reallocation might copy the
        -- bytes anyway, we could discard the consumed bytes,
        -- basically 'reset'ting the buffer on the fly.
        ptr' <- Alloc.reallocBytes (ptr bbref) newSize
        return BBRef { size = newSize
                     , contained = contained bbref
                     , consumed = consumed bbref
                     , ptr = ptr'
                     }

-- | Copy the contents of a 'ByteString' to a 'ByteBuffer'.
--
-- If necessary, the 'ByteBuffer' is enlarged and/or already consumed
-- bytes are dropped.
copyByteString :: MonadIO m => ByteBuffer -> ByteString -> m ()
copyByteString bb bs =
    bbHandler "copyByteString" bb go
  where
    go bbref = do
        let (bsFptr, bsOffset, bsSize) = BS.toForeignPtr bs
        -- if the byteBuffer is too small, resize it.
        let available = contained bbref - consumed bbref -- bytes not yet consumed
        bbref' <- if size bbref < bsSize + available
                  then enlargeBBRef bbref (bsSize + available)
                  else return bbref
        -- if it is currently too full, reset it
        bbref'' <- if bsSize + contained bbref' > size bbref'
                   then resetBBRef bbref'
                   else return bbref'
        -- now we can safely copy.
        withForeignPtr bsFptr $ \ bsPtr ->
            copyBytes (ptr bbref'' `plusPtr` contained bbref'')
            (bsPtr `plusPtr` bsOffset)
            bsSize
        writeIORef bb $ Right BBRef {
            size = size bbref''
            , contained = contained bbref'' + bsSize
            , consumed = consumed bbref''
            , ptr = ptr bbref''}

#ifndef mingw32_HOST_OS

-- | Will read at most n bytes from the given 'Fd', in a non-blocking
-- fashion. This function is intended to be used with non-blocking 'Socket's,
-- such the ones created by the @network@ package.
--
-- Returns how many bytes could be read non-blockingly.
fillFromFd :: (MonadIO m, Fail.MonadFail m) => ByteBuffer -> Fd -> Int -> m Int
fillFromFd bb sock maxBytes = if maxBytes < 0
    then fail ("fillFromFd: negative argument (" ++ show maxBytes ++ ")")
    else bbHandler "fillFromFd" bb go
  where
    go bbref = do
        (bbref', readBytes) <- fillBBRefFromFd sock bbref maxBytes
        writeIORef bb $ Right bbref'
        return readBytes

{-
Note: I'd like to use these two definitions:

{-@ measure _available @-}
_available :: BBRef -> Int
_available BBRef{..} = contained - consumed

{-@ measure _free @-}
_free :: BBRef -> Int
_free BBRef{..} = size - contained

but for some reason when I try to do so it does not work.
-}

{-@ fillBBRefFromFd ::
       Fd -> b0: BBRef
    -> maxBytes: Nat -> IO {v: (BBRef, Nat) | maxBytes >= snd v && contained (fst v) - consumed (fst v) == contained b0 - consumed b0 + snd v}
  @-}
fillBBRefFromFd :: Fd -> BBRef -> Int -> IO (BBRef, Int)
fillBBRefFromFd (Fd sock) bbref0 maxBytes = do
  bbref1 <- prepareSpace bbref0
  go 0 bbref1
  where
    -- We enlarge the buffer directly to be able to contain the maximum number
    -- of bytes since the common pattern for this function is to know how many
    -- bytes we need to read -- so we'll eventually fill the enlarged buffer.
    {-@ prepareSpace :: b: BBRef -> IO {v: BBRef | size v - contained v >= maxBytes && contained b - consumed b == contained v - consumed v} @-}
    prepareSpace :: BBRef -> IO BBRef
    prepareSpace bbref = do
      let space = size bbref - contained bbref
      if space < maxBytes
        then if consumed bbref > 0
          then prepareSpace =<< resetBBRef bbref
          else enlargeBBRef bbref (contained bbref + maxBytes)
        else return bbref

    {-@ go ::
           readBytes: {v: Nat | v <= maxBytes}
        -> b: {b: BBRef | size b - contained b >= maxBytes - readBytes}
        -> IO {v: (BBRef, Nat) | maxBytes >= snd v && snd v >= readBytes && size (fst v) - contained (fst v) >= maxBytes - snd v && contained (fst v) - consumed (fst v) == (contained b - consumed b) + (snd v - readBytes)}
      @-}
    go :: Int -> BBRef -> IO (BBRef, Int)
    go readBytes bbref@BBRef{..} = if readBytes >= maxBytes
      then return (bbref, readBytes)
      else do
        bytes <- fromIntegral <$> c_recv sock (castPtr (ptr `plusPtr` contained)) (fromIntegral (maxBytes - readBytes)) 0
        if bytes == -1
          then do
            err <- CE.getErrno
            if err == CE.eAGAIN || err == CE.eWOULDBLOCK
              then return (bbref, readBytes)
              else throwIO $ CE.errnoToIOError "ByteBuffer.fillBBRefFromFd: " err Nothing Nothing
          else do
            let bbref' = bbref{ contained = contained + bytes }
            go (readBytes + bytes) bbref'

foreign import ccall unsafe "recv"
  -- c_recv returns -1 in the case of errors.
  {-@ assume c_recv :: CInt -> Ptr CChar -> size: {v: CSize | v >= 0} -> flags: CInt -> IO {read: CInt | read >= -1 && size >= read} @-}
  c_recv :: CInt -> Ptr CChar -> CSize -> CInt -> IO CInt

#endif

-- | Try to get a pointer to @n@ bytes from the 'ByteBuffer'.
--
-- Note that the pointer should be used before any other actions are
-- performed on the 'ByteBuffer'. It points to some address within the
-- buffer, so operations such as enlarging the buffer or feeding it
-- new data will change the data the pointer points to.  This is why
-- this function is called unsafe.
{-@ unsafeConsume :: MonadIO m => ByteBuffer -> n:Nat -> m (Either Int ({v:Ptr Word8 | plen v >= n})) @-}
unsafeConsume :: MonadIO m
        => ByteBuffer
        -> Int
        -- ^ n
        -> m (Either Int (Ptr Word8))
        -- ^ Will be @Left missing@ when there are only @n-missing@
        -- bytes left in the 'ByteBuffer'.
unsafeConsume bb n =
    bbHandler "unsafeConsume" bb go
  where
    go bbref = do
        let available = contained bbref - consumed bbref
        if available < n
            then return $ Left (n - available)
            else do
                writeIORef bb $ Right bbref { consumed = consumed bbref + n }
                return $ Right (ptr bbref `plusPtr` consumed bbref)

-- | As `unsafeConsume`, but instead of returning a `Ptr` into the
-- contents of the `ByteBuffer`, it returns a `ByteString` containing
-- the next @n@ bytes in the buffer.  This involves allocating a new
-- 'ByteString' and copying the @n@ bytes to it.
{-@ consume :: MonadIO m => ByteBuffer -> Nat -> m (Either Int ByteString) @-}
consume :: MonadIO m
        => ByteBuffer
        -> Int
        -> m (Either Int ByteString)
consume bb n = do
    mPtr <- unsafeConsume bb n
    case mPtr of
        Right ptr -> do
            bs <- liftIO $ createBS ptr n
            return (Right bs)
        Left missing -> return (Left missing)

{-@ createBS :: p:(Ptr Word8) -> {v:Nat | v <= plen p} -> IO ByteString @-}
createBS :: Ptr Word8 -> Int -> IO ByteString
createBS ptr n = do
  fp  <- mallocForeignPtrBytes n
  withForeignPtr fp (\p -> copyBytes p ptr n)
  return (BS.PS fp 0 n)

-- below are liquid haskell qualifiers, and specifications for external functions.

{-@ qualif FPLenPLen(v:Ptr a, fp:ForeignPtr a): fplen fp = plen v @-}

{-@ Foreign.Marshal.Alloc.mallocBytes :: l:Nat -> IO (PtrN a l) @-}
{-@ Foreign.Marshal.Alloc.reallocBytes :: Ptr a -> l:Nat -> IO (PtrN a l) @-}
{-@ assume mallocForeignPtrBytes :: n:Nat -> IO (ForeignPtrN a n) @-}
{-@ type ForeignPtrN a N = {v:ForeignPtr a | fplen v = N} @-}
{-@ Foreign.Marshal.Utils.copyBytes :: p:Ptr a -> q:Ptr a -> {v:Nat | v <= plen p && v <= plen q} -> IO ()@-}
{-@ Foreign.Marshal.Utils.moveBytes :: p:Ptr a -> q:Ptr a -> {v:Nat | v <= plen p && v <= plen q} -> IO ()@-}
{-@ Foreign.Ptr.plusPtr :: p:Ptr a -> n:Nat -> {v:Ptr b | plen v == (plen p) - n} @-}

-- writing down the specification for ByteString is not as straightforward as it seems at first: the constructor
--
-- PS (ForeignPtr Word8) Int Int
--
-- has actually four arguments after unboxing (the ForeignPtr is an
-- Addr# and ForeignPtrContents), so restriciting the length of the
-- ForeignPtr directly in the specification of the datatype does not
-- work.  Instead, I chose to write a specification for toForeignPtr.
-- It seems that the liquidhaskell parser has problems with variables
-- declared in a tuple, so I have to define the following measures to
-- get at the ForeignPtr, offset, and length.
--
-- This is a bit awkward, maybe there is an easier way.

_get1 :: (a,b,c) -> a
_get1 (x,_,_) = x
{-@ measure _get1 @-}
_get2 :: (a,b,c) -> b
_get2 (_,x,_) = x
{-@ measure _get2 @-}
_get3 :: (a,b,c) -> c
_get3 (_,_,x) = x
{-@ measure _get3 @-}

{-@ Data.ByteString.Internal.toForeignPtr :: ByteString ->
  {v:(ForeignPtr Word8, Int, Int) | _get2 v >= 0
                                 && _get2 v <= (fplen (_get1 v))
                                 && _get3 v >= 0
                                 && ((_get3 v) + (_get2 v)) <= (fplen (_get1 v))} @-}
