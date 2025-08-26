-- |
-- High-performance composable binary data deserializers.
--
-- This module provides two types of decoders for parsing binary data:
--
-- * 'Fixed' decoders for compile-time known, fixed-size data structures
-- * 'Variable' decoders for runtime-dependent, variable-size data structures
--
-- Both types support full 'Applicative' and 'Monad' composition, enabling
-- elegant construction of complex binary parsers with superior performance.
--
-- == Quick Start
--
-- @
-- import PtrPeeker
--
-- -- Decode a fixed-size record
-- data Point = Point Int32 Int32 Int32
-- pointDecoder = Point \<$\> beSignedInt4 \<*\> beSignedInt4 \<*\> beSignedInt4
--
-- -- Decode variable-length data
-- variableString = do
--   len <- fixed beUnsignedInt4
--   fixed (byteArrayAsByteString (fromIntegral len))
--
-- -- Execute decoders
-- result1 = decodeByteStringWithFixed pointDecoder bytes
-- result2 = decodeByteStringWithVariable variableString bytes
-- @
module PtrPeeker
  ( -- * Execution
    decodeByteStringWithVariable,
    decodeByteStringWithVariableWithRemainders,
    decodeByteStringWithFixed,
    decodePtrWithVariableWithRemainders,

    -- * Variable
    Variable,
    hasMore,
    forceSize,
    fixed,
    nullTerminatedStringAsByteString,
    nullTerminatedStringAsShortByteString,
    variableArray,
    remainderAsByteString,

    -- * Fixed
    Fixed,
    skip,
    storable,

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

import PtrPeeker.Fixed
import PtrPeeker.Variable
