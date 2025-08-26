-- |
-- High-performance composable binary data deserializers.
--
-- This module provides two types of decoders for parsing binary data:
--
-- * 'Fixed' decoders for compile-time known, fixed-size data structures
-- * 'Variable' decoders for runtime-dependent, variable-size data structures
--
-- == Quick Start
--
-- > import PtrPeeker
-- > import qualified Data.Vector
-- >
-- > data Point = Point Int32 Int32 Int32
-- >
-- > point :: Variable Point
-- > point =
-- >   fixed (Point <$> beSignedInt4 <*> beSignedInt4 <*> beSignedInt4)
-- >
-- > points :: Variable (Data.Vector.Vector Point)
-- > points = do
-- >   count <- fixed beUnsignedInt4
-- >   Data.Vector.replicateM (fromIntegral count) point
-- >
-- > decodePoint :: ByteString -> Either Int (Data.Vector.Vector Point)
-- > decodePoint = runVariableOnByteString points
module PtrPeeker
  ( -- * Execution
    runVariableOnByteString,
    runVariableOnByteStringWithRemainders,
    runVariableOnPtrWithRemainders,
    runFixedOnByteString,

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
