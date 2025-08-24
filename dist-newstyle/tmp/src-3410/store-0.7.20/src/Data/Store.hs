-- | This is the main public API of the store package. The functions
-- exported here are more likely to be stable between versions.
--
-- Usually you won't need to write your own 'Store' instances, and
-- instead can rely on either using the 'Generic' deriving approach or
-- "Data.Store.TH" for defining 'Store' instances for your datatypes.
-- There are some tradeoffs here - the generics instances do not require
-- @-XTemplateHaskell@, but they do not optimize as well for sum types
-- that only require a constant number of bytes.
--
-- If you need streaming encode / decode of multiple store encoded
-- messages, take a look at the @store-streaming@ package.
--
-- = Gotchas
--
-- Store is best used for communication between trusted processes and
-- local caches.  It can certainly be used for other purposes, but the
-- builtin set of instances have some gotchas to be aware of:
--
-- * Store's builtin instances serialize in a format which depends on
--   machine endianness.
--
-- * Store's builtin instances trust the data when deserializing. For
--   example, the deserialization of `Vector` will read the vector's
--   link from the first 8 bytes. It will then allocate enough memory
--   to store all the elements. Malicious or malformed input could
--   cause allocation of large amounts of memory.  See
--   https://github.com/fpco/store/issues/122
module Data.Store
    (
    -- * Encoding and decoding strict ByteStrings.
      encode,
      decode, decodeWith,
      decodeEx, decodeExWith, decodeExPortionWith,
      decodeIO, decodeIOWith, decodeIOPortionWith
    -- * Store class and related types.
    , Store(..), Size(..), Poke, Peek
    , GStoreSize, GStorePoke, GStorePeek
    -- ** Exceptions thrown by Peek
    , PeekException(..), peekException
    ) where

import Data.Store.Internal
