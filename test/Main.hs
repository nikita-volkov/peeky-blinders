module Main where

import qualified Data.Serialize as Cereal
import qualified PeekyBlinders as Pb
import Test.QuickCheck
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.Runners
import Prelude hiding (all, choose)

main = defaultMain $ testGroup "All" all

all =
  [ testCase "Unterminated C-string" $ do
      assertEqual "" Nothing $
        either (const Nothing) Just $
          Pb.decodeByteString Pb.byteStringTerminatedByNull "\1\2\3\4",
    testCase "Terminated C-string" $ do
      assertEqual "" (Right "abc") $
        Pb.decodeByteString Pb.byteStringTerminatedByNull "abc\0d",
    testCase "Composition after C-string" $ do
      assertEqual "" (Right ("abc", "def")) $
        flip Pb.decodeByteString "abc\0def\0" $
          (,) <$> Pb.byteStringTerminatedByNull <*> Pb.byteStringTerminatedByNull
  ]
