module Main where

import Prelude hiding (choose)
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances
import qualified PeekyBlinders
import qualified Data.Serialize as Cereal


main =
  defaultMain $ testGroup "All" [
    ]
