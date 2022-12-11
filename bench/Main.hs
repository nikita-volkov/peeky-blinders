import Criterion.Main
import qualified Data.Persist as Persist
import qualified Data.Serialize as Cereal
import qualified Data.Store as Store
import Data.String.ToString
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as Vu
import qualified PeekyBlinders as Pb
import qualified Test.Tasty.HUnit as Tasty
import Prelude

main = do
  putStrLn "Testing"
  groups <-
    sequence
      [ let input = Cereal.runPut $ do
              Cereal.putInt32le 1
              Cereal.putInt32le 2
              Cereal.putInt32le 3
            correctDecoding = (1, 2, 3)
            subjects =
              [ ( "peeky-blinders/statically",
                  hush . Pb.decodeByteStringDynamically (Pb.statically $ (,,) <$> Pb.leSignedInt4 <*> Pb.leSignedInt4 <*> Pb.leSignedInt4)
                ),
                ( "peeky-blinders/dynamically",
                  hush . Pb.decodeByteStringDynamically ((,,) <$> Pb.statically Pb.leSignedInt4 <*> Pb.statically Pb.leSignedInt4 <*> Pb.statically Pb.leSignedInt4)
                ),
                ( "store",
                  hush . Store.decode @(Int32, Int32, Int32)
                ),
                ( "cereal",
                  hush . Cereal.runGet ((,,) <$> Cereal.getInt32le <*> Cereal.getInt32le <*> Cereal.getInt32le)
                ),
                ( "persist",
                  hush . Persist.runGet ((,,) <$> Persist.getLE @Int32 <*> Persist.getLE @Int32 <*> Persist.getLE @Int32)
                )
              ]
         in initGroup "int32-le-triplet" input correctDecoding subjects,
        let input =
              Cereal.runPut $
                Cereal.putInt32le 100 <> replicateM_ 100 (Cereal.putInt32le (-1))
            correctDecoding =
              Vu.replicate 100 (-1)
            subjects =
              [ ( "peeky-blinders",
                  let decoder = do
                        size <- Pb.statically Pb.leSignedInt4
                        Pb.statically $ Pb.staticArray @Vu.Vector Pb.leSignedInt4 $ fromIntegral size
                   in hush . Pb.decodeByteStringDynamically decoder
                ),
                ( "store",
                  let decoder = do
                        size <- Store.peek @Int32
                        Vu.replicateM (fromIntegral size) $ Store.peek @Int32
                   in hush . Store.decodeWith decoder
                ),
                ( "cereal",
                  let decoder = do
                        size <- Cereal.getInt32le
                        Vu.replicateM (fromIntegral size) $ Cereal.getInt32le
                   in hush . Cereal.runGet decoder
                )
              ]
         in initGroup "array-of-int4" input correctDecoding subjects,
        let input = Cereal.runPut $ do
              Cereal.putInt64le 100
              replicateM_ 100 $ do
                Cereal.putInt64le 3
                Cereal.putByteString "abc"
            correctDecoding = V.replicate 100 "abc"
            subjects =
              [ ( "peeky-blinders",
                  let decoder = do
                        size <- Pb.statically Pb.leSignedInt8
                        Pb.dynamicArray @V.Vector byteStringDecoder $ fromIntegral size
                      byteStringDecoder = do
                        size <- Pb.statically Pb.leSignedInt8
                        Pb.statically $ Pb.byteArrayAsByteString $ fromIntegral size
                   in hush . Pb.decodeByteStringDynamically decoder
                ),
                ( "store",
                  hush . Store.decode
                ),
                ( "cereal",
                  let decoder = do
                        size <- Cereal.getInt64le
                        V.replicateM (fromIntegral size) $ do
                          size <- Cereal.getInt64le
                          Cereal.getByteString $ fromIntegral size
                   in hush . Cereal.runGet decoder
                )
              ]
         in initGroup "array-of-byte-arrays" input correctDecoding subjects
      ]

  putStrLn "Benchmarking"
  defaultMain groups

-- | Test functions and create a benchmark group out of them.
initGroup :: (Eq a, Show a, NFData a) => String -> ByteString -> a -> [(String, ByteString -> Maybe a)] -> IO Benchmark
initGroup name input correctDecoding subjects = do
  fmap (bgroup name) . forM subjects $ \(name, f) -> do
    Tasty.assertEqual name (Just correctDecoding) (f input)
    return $ bench name $ nf f input

-- | Suppress the 'Left' value of an 'Either'
hush :: Either a b -> Maybe b
hush = either (const Nothing) Just
