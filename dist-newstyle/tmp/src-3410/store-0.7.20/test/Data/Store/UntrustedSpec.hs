{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for untrusted data.

module Data.Store.UntrustedSpec where

import           Test.Hspec

spec :: Spec
spec = return ()

{- Untrusted data spec is disabled for now.  See #122 / #123 for details

import           Data.Bifunctor
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.Int
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Monoid
import           Data.Proxy
import qualified Data.Sequence as Seq
import           Data.Store
import           Data.Store.Internal
import           Data.String
import           Data.Text (Text)
import qualified Data.Vector as V

-- | Test suite.
actualSpec :: Spec
actualSpec =
    describe
        "Untrusted input throws error"
        (do describe
                "Array-like length prefixes"
                (do let sample
                            :: IsString s
                            => s
                        sample = "abc"
                        list :: [Int]
                        list = [1, 2, 3]
                    it
                        "ByteString"
                        (shouldBeRightWrong huge (sample :: ByteString))
                    it
                        "Lazy ByteString"
                        (shouldBeRightWrong huge (sample :: L.ByteString))
                    it "Text" (shouldBeRightWrong huge (sample :: Text))
                    it "String" (shouldBeRightWrong huge (sample :: String))
                    it "Vector Int" (shouldBeRightWrong huge (V.fromList list))
                    it
                        "Vector Char"
                        (shouldBeRightWrong huge (V.fromList (sample :: [Char])))
                    it
                        "Vector unit"
                        (shouldBeRightWrong
                             huge
                             (V.fromList (replicate 1000 ())))
                    it "Seq Int" (shouldBeRightWrong huge (Seq.fromList (sample :: [Char]))))
            describe
                "Maps are consistent"
                (do it
                        "Map Int Int: with duplicates"
                        (shouldBeFail
                             (DuplicatedMap
                                  (M.fromList [(1, 2), (4, 5)] :: Map Int Int))
                             (Proxy :: Proxy (Map Int Int)))
                    it
                        "Map Int Int: wrong order"
                        (shouldBeFail
                             (ReversedMap
                                  (M.fromList [(1, 2), (4, 5)] :: Map Int Int))
                             (Proxy :: Proxy (Map Int Int)))
                    it
                        "IntMap Int Int: with duplicates"
                        (shouldBeFail
                             (DuplicatedIntMap
                                  (IM.fromList [(1, 2), (4, 5)] :: IntMap Int))
                             (Proxy :: Proxy (IntMap Int)))
                    it
                        "IntMap Int Int: wrong order"
                        (shouldBeFail
                             (ReversedIntMap
                                  (IM.fromList [(1, 2), (4, 5)] :: IntMap Int))
                             (Proxy :: Proxy (IntMap Int))))
            describe
                "Constructor tags"
                (do it
                        "Invalid constructor tag"
                        (shouldBe
                             (first
                                  (const ())
                                  (decode "\2" :: Either PeekException (Maybe ())))
                             (Left ()))
                    it
                        "Missing slots"
                        (shouldBe
                             (first
                                  (const ())
                                  (decode "\1" :: Either PeekException (Maybe Char)))
                             (Left ()))))

huge :: Int64
huge = 2^(62::Int)

-- | Check decode.encode==id and then check decode.badencode=>error.
shouldBeRightWrong
    :: forall i.
       (Store i, Eq i, Show i)
    => Int64 -> i -> IO ()
shouldBeRightWrong len input = do
    shouldBe (decode (encode input) :: Either PeekException i) (Right input)
    shouldBe
        (first
             (const ())
             (decode (encodeWrongPrefix len input) :: Either PeekException i))
        (Left ())

-- | Check decode.encode==id and then check decode.badencode=>error.
shouldBeFail
    :: forall o i.
       (Store i, Eq o, Show o, Store o)
    => i -> Proxy o -> IO ()
shouldBeFail input _ =
    shouldBe
        (first
             (const ())
             (decode (encode input) :: Either PeekException o))
        (Left ())

-- | Encode a thing with the wrong length prefix.
encodeWrongPrefix :: Store thing => Int64 -> thing -> ByteString
encodeWrongPrefix len thing = encode len <> encodeThingNoPrefix thing

-- | Encode the thing and drop the length prefix.
encodeThingNoPrefix :: Store thing => thing -> ByteString
encodeThingNoPrefix = S.drop (S.length (encode (1 :: Int64))) . encode

--------------------------------------------------------------------------------
-- Map variants

newtype ReversedIntMap = ReversedIntMap (IntMap Int)
  deriving (Show, Eq)
instance Store ReversedIntMap where
    poke (ReversedIntMap m) = do
        poke markMapPokedInAscendingOrder
        poke (reverse (IM.toList m))
    peek = error "ReversedIntMap.peek"
    size = VarSize (\(ReversedIntMap m) -> getSize m)

newtype DuplicatedIntMap = DuplicatedIntMap (IntMap Int)
  deriving (Show, Eq)
instance Store DuplicatedIntMap where
    poke (DuplicatedIntMap m) = do
        poke markMapPokedInAscendingOrder
        poke (let xs = IM.toList m
              in take (length xs) (cycle (take 1 xs)))
    peek = error "DuplicatedIntMap.peek"
    size = VarSize (\(DuplicatedIntMap m) -> getSize m)

newtype ReversedMap = ReversedMap (Map Int Int)
  deriving (Show, Eq)
instance Store ReversedMap where
    poke (ReversedMap m) = do
        poke markMapPokedInAscendingOrder
        poke (reverse (M.toList m))
    peek = error "ReversedMap.peek"
    size = VarSize (\(ReversedMap m) -> getSize m)

newtype DuplicatedMap = DuplicatedMap (Map Int Int)
  deriving (Show, Eq)
instance Store DuplicatedMap where
    poke (DuplicatedMap m) = do
        poke markMapPokedInAscendingOrder
        poke (let xs = M.toList m
              in take (length xs) (cycle (take 1 xs)))
    peek = error "DuplicatedMap.peek"
    size = VarSize (\(DuplicatedMap m) -> getSize m)

-}
