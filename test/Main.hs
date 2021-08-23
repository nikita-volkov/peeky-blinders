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
  [
  ]
