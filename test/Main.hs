module Main where

import qualified PeekyBlinders as Pb
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.HUnit
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
          (,) <$> Pb.nullTerminatedStringAsByteString <*> Pb.nullTerminatedStringAsByteString
  ]
