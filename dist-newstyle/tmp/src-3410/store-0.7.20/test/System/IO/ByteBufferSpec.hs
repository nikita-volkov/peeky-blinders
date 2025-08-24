{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module System.IO.ByteBufferSpec where

import           Control.Exception
import qualified Data.ByteString as BS
import           Data.Typeable (Typeable)
import qualified System.IO.ByteBuffer as BB
import           Test.Hspec

data MyException = MyException
    deriving (Eq, Show, Typeable)
instance Exception MyException

spec :: Spec
spec = describe "ByteBuffer" $ do
    it "can grow to store a value and return it." $ BB.with (Just 0) $ \ bb -> do
        let bs = "some bytestring"
        BB.copyByteString bb bs
        bs' <- BB.consume bb (BS.length bs)
        bs' `shouldBe` Right bs
        bbIsEmpty bb
    it "should request more input when needed." $ BB.with (Just 0) $ \ bb -> do
        let bs = "some bytestring"
        BB.copyByteString bb bs
        bs' <- BB.consume bb (2 * BS.length bs)
        bs' `shouldBe` Left (BS.length bs)
        BB.copyByteString bb bs
        bs'' <- BB.consume bb (2 * BS.length bs)
        bs'' `shouldBe` Right (BS.concat [bs, bs])
        bbIsEmpty bb
    it "should not grow if bytes can be freed." $
        let bs1 = "12345"
            bs2 = "67810" -- what about nine? 7 8 9!
        in BB.with (Just $ BS.length bs1) $ \ bb -> do
            BB.copyByteString bb bs1
            bs1' <- BB.consume bb (BS.length bs1)
            BB.copyByteString bb bs2
            bs2' <- BB.consume bb (BS.length bs2)
            bs1' `shouldBe` Right bs1
            bs2' `shouldBe` Right bs2
            bbSize <- BB.totalSize bb
            bbSize `shouldBe` BS.length bs1
            bbIsEmpty bb
    it "should raise a ByteBufferException when used after freed" $ BB.with Nothing $ \bb -> do
        BB.free bb
        BB.totalSize bb `shouldThrow` \(BB.ByteBufferException loc e) ->
            loc == "free" && e == "ByteBuffer has explicitly been freed and is no longer valid."
    it "should raise a ByteBufferException after a failed operation" $ BB.with Nothing $ \bb -> do
        BB.copyByteString bb (throw MyException) `shouldThrow` (\MyException -> True)
        BB.consume bb 10 `shouldThrow` \(BB.ByteBufferException loc e) ->
            loc == "copyByteString" && e == show MyException
bbIsEmpty :: BB.ByteBuffer -> Expectation
bbIsEmpty bb = BB.isEmpty bb >>= (`shouldBe` True)
