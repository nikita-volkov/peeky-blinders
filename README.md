# ptr-peeker

A high-performance binary data deserialization library for Haskell that provides composable decoders for both fixed-size and variable-size binary data structures.

## Features

- **High Performance**: Outperforms existing libraries like cereal and store in benchmarks
- **Composable**: `Applicative` and `Monad` for elegant parser construction  
- **Type-Safe**: Leverages Haskell's type system to prevent common binary parsing errors

## Quick Start

```haskell
import PtrPeeker
import qualified Data.Vector

-- Decode a fixed-size record
data Point = Point Int32 Int32 Int32

point :: Fixed Point  
point = Point <$> beSignedInt4 <*> beSignedInt4 <*> beSignedInt4

points :: Variable (Data.Vector.Vector Point)
points = do
  count <- fixedly beUnsignedInt4
  Data.Vector.replicateM (fromIntegral count) (fixedly point)

-- Execute decoders
decodePoint :: ByteString -> Either Int Point
decodePoint = decodeByteStringFixedly point
```

# Benchmarks

```
benchmarking int32-le-triplet/ptr-peeker/fixedly
time                 14.89 ns   (14.71 ns .. 15.10 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 14.77 ns   (14.71 ns .. 14.94 ns)
std dev              294.0 ps   (210.5 ps .. 498.3 ps)
variance introduced by outliers: 30% (moderately inflated)

benchmarking int32-le-triplet/ptr-peeker/variably
time                 16.80 ns   (16.74 ns .. 16.90 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 16.84 ns   (16.74 ns .. 17.02 ns)
std dev              429.2 ps   (264.3 ps .. 688.6 ps)
variance introduced by outliers: 41% (moderately inflated)

benchmarking int32-le-triplet/store
time                 122.3 ns   (121.7 ns .. 122.8 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 122.7 ns   (122.1 ns .. 123.3 ns)
std dev              1.792 ns   (1.347 ns .. 2.330 ns)
variance introduced by outliers: 16% (moderately inflated)

benchmarking int32-le-triplet/cereal
time                 21.50 ns   (21.39 ns .. 21.63 ns)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 21.68 ns   (21.56 ns .. 22.07 ns)
std dev              628.8 ps   (301.6 ps .. 1.124 ns)
variance introduced by outliers: 47% (moderately inflated)

benchmarking array-of-int4/ptr-peeker
time                 107.3 ns   (106.9 ns .. 107.7 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 107.3 ns   (107.0 ns .. 107.7 ns)
std dev              1.351 ns   (1.063 ns .. 2.099 ns)
variance introduced by outliers: 13% (moderately inflated)

benchmarking array-of-int4/store
time                 898.5 ns   (894.1 ns .. 903.7 ns)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 903.0 ns   (898.1 ns .. 911.5 ns)
std dev              23.45 ns   (12.77 ns .. 39.93 ns)
variance introduced by outliers: 34% (moderately inflated)

benchmarking array-of-int4/cereal
time                 2.130 μs   (2.107 μs .. 2.157 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 2.122 μs   (2.112 μs .. 2.139 μs)
std dev              41.90 ns   (32.47 ns .. 54.31 ns)
variance introduced by outliers: 22% (moderately inflated)

benchmarking array-of-byte-arrays/ptr-peeker
time                 2.071 μs   (2.063 μs .. 2.080 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.074 μs   (2.066 μs .. 2.084 μs)
std dev              31.68 ns   (24.10 ns .. 42.63 ns)
variance introduced by outliers: 15% (moderately inflated)

benchmarking array-of-byte-arrays/store
time                 3.242 μs   (3.215 μs .. 3.277 μs)
                     0.999 R²   (0.996 R² .. 1.000 R²)
mean                 3.238 μs   (3.217 μs .. 3.314 μs)
std dev              129.0 ns   (49.67 ns .. 256.7 ns)
variance introduced by outliers: 52% (severely inflated)

benchmarking array-of-byte-arrays/cereal
time                 3.780 μs   (3.752 μs .. 3.818 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 3.795 μs   (3.766 μs .. 3.843 μs)
std dev              124.9 ns   (84.13 ns .. 171.3 ns)
variance introduced by outliers: 43% (moderately inflated)
```