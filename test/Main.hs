module Main where

import qualified Data.Serialize as Cereal
import qualified Data.Vector.Unboxed as Vu
import qualified PeekyBlinders as Pb
import qualified Test.QuickCheck as Qc
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Prelude hiding (all, choose)

main = defaultMain $ testGroup "All" all

all =
  [ testCase "Unterminated C-string" $ do
      assertEqual "" Nothing $
        either (const Nothing) Just $
          Pb.decodeByteString Pb.nullTerminatedStringAsByteString "\1\2\3\4",
    testCase "Terminated C-string" $ do
      assertEqual "" (Right "abc") $
        Pb.decodeByteString Pb.nullTerminatedStringAsByteString "abc\0d",
    testCase "Composition after C-string" $ do
      assertEqual "" (Right ("abc", "def")) $
        flip Pb.decodeByteString "abc\0def\0" $
          (,) <$> Pb.nullTerminatedStringAsByteString <*> Pb.nullTerminatedStringAsByteString,
    testProperty "staticArray" $ do
      vec <- Qc.arbitrary @(Vu.Vector Int32)
      let bs = Cereal.runPut $ do
            Cereal.putInt32be $ fromIntegral $ Vu.length vec
            Vu.forM_ vec $ Cereal.putInt32be
          res = flip Pb.decodeByteString bs $ do
            size <- Pb.statically Pb.beSignedInt4
            Pb.statically $ Pb.staticArray Pb.beSignedInt4 $ fromIntegral size
      return $ Right vec == res
  ]
