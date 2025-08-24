module Main where

import Data.ByteString qualified as Bs
import Data.Serialize qualified as Cereal
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as Vu
import PtrPeeker qualified as Pb
import Test.QuickCheck qualified as Qc
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Prelude hiding (all, choose)

main :: IO ()
main =
  defaultMain
    $ testGroup "All"
    $ [ testCase "Unterminated C-string" $ do
          assertEqual "" Nothing
            $ either (const Nothing) Just
            $ Pb.decodeByteStringDynamically Pb.nullTerminatedStringAsByteString "\1\2\3\4",
        testCase "Terminated C-string" $ do
          assertEqual "" (Right "abc")
            $ Pb.decodeByteStringDynamically Pb.nullTerminatedStringAsByteString "abc\0d",
        testCase "Composition after C-string" $ do
          assertEqual "" (Right ("abc", "def"))
            $ flip Pb.decodeByteStringDynamically "abc\0def\0"
            $ (,)
            <$> Pb.nullTerminatedStringAsByteString
            <*> Pb.nullTerminatedStringAsByteString,
        testProperty "staticArray" $ do
          vec <- Qc.arbitrary @(Vu.Vector Int32)
          let bs = Cereal.runPut $ do
                Cereal.putInt32be $ fromIntegral $ Vu.length vec
                Vu.forM_ vec $ Cereal.putInt32be
              res = flip Pb.decodeByteStringDynamically bs $ do
                size <- Pb.statically Pb.beSignedInt4
                Pb.statically $ Pb.staticArray Pb.beSignedInt4 $ fromIntegral size
          return $ Right vec == res,
        testProperty "dynamicArray" $ do
          size <- Qc.choose (0, 99)
          vec <- V.replicateM (fromIntegral size) $ do
            size <- Qc.choose (0, 99)
            byteList <- replicateM size $ Qc.choose (1, 255)
            return $ Bs.pack byteList
          let bs = Cereal.runPut $ do
                Cereal.putInt32be size
                V.forM_ vec $ \x -> do
                  Cereal.putByteString x
                  Cereal.putWord8 0
              res = flip Pb.decodeByteStringDynamically bs $ do
                size <- Pb.statically Pb.beSignedInt4
                Pb.dynamicArray Pb.nullTerminatedStringAsByteString $ fromIntegral size
          return $ Right vec == res,
        testCase "forceSize" $ do
          assertEqual "" (Left 1)
            $ Pb.decodeByteStringDynamically (Pb.forceSize 3 (Pb.statically Pb.beSignedInt4)) "\1\2\3\4"
          let bs = Cereal.runPut $ do
                Cereal.putInt32be 5
                Cereal.putWord8 0
                Cereal.putWord8 0
                Cereal.putWord8 0
                Cereal.putInt32be 7
              dec = do
                a <- Pb.forceSize 7 $ Pb.statically Pb.beSignedInt4
                b <- Pb.statically Pb.beSignedInt4
                return (a, b)
           in assertEqual "" (Right (5, 7)) $ Pb.decodeByteStringDynamically dec bs
          assertEqual "" (Right 1)
            $ Pb.decodeByteStringDynamically (Pb.forceSize 4 (Pb.statically Pb.beSignedInt4)) "\0\0\0\1"
      ]
