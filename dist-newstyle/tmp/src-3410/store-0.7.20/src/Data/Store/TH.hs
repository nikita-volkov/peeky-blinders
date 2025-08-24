{-# LANGUAGE TemplateHaskell #-}

-- | This module exports TH utilities intended to be useful to users.
--
-- 'makeStore' can be used to generate a 'Store' instance for types,
-- when all the type variables also require 'Store' instances. If some
-- do not, then instead use "TH.Derive" like this:
--
-- @
-- \{\-\# LANGUAGE TemplateHaskell \#\-\}
-- \{\-\# LANGUAGE ScopedTypeVariables \#\-\}
--
-- import TH.Derive
-- import Data.Store
--
-- data Foo a = Foo a | Bar Int
--
-- \$($(derive [d|
--     instance Store a => Deriving (Store (Foo a))
--     |]))
-- @
--
-- Note that when used with datatypes that require type variables, the
-- ScopedTypeVariables extension is required.
--
-- One advantage of using this Template Haskell definition of Store
-- instances is that in some cases they can be faster than the instances
-- defined via Generics. Specifically, sum types which can yield
-- 'ConstSize' from 'size' will be faster when used in array-like types.
-- The instances generated via generics always use 'VarSize' for sum
-- types.
module Data.Store.TH
    ( makeStore
    -- * Testing Store instances
    , smallcheckManyStore
    , checkRoundtrip
    , assertRoundtrip
    ) where

import qualified Control.Monad.Fail as Fail
import Data.Complex ()
import Data.Store.Impl
import Data.Typeable (Typeable, typeOf)
import Debug.Trace (trace)
import Language.Haskell.TH
import Prelude
import Test.Hspec
import Test.Hspec.SmallCheck (property)
import Test.SmallCheck
import Data.Store.TH.Internal (makeStore)

------------------------------------------------------------------------
-- Testing

-- | Test a 'Store' instance using 'smallcheck' and 'hspec'.
smallcheckManyStore :: Bool -> Int -> [TypeQ] -> ExpQ
smallcheckManyStore verbose depth = smallcheckMany . map testRoundtrip
  where
    testRoundtrip tyq = do
        ty <- tyq
        expr <- [e| property $ changeDepth (\_ -> depth) $ \x -> checkRoundtrip verbose (x :: $(return ty)) |]
        return ("Roundtrips (" ++ pprint ty ++ ")", expr)

assertRoundtrip :: (Eq a, Show a, Store a, Fail.MonadFail m, Typeable a) => Bool -> a -> m ()
assertRoundtrip verbose x
    | checkRoundtrip verbose x = return ()
    | otherwise = fail $ "Failed to roundtrip "  ++ show (typeOf x)

-- | Check if a given value succeeds in decoding its encoded
-- representation.
checkRoundtrip :: (Eq a, Show a, Store a) => Bool -> a -> Bool
checkRoundtrip verbose x = decoded == Right x
  where
    encoded = verboseTrace verbose "encoded" (encode x)
    decoded = verboseTrace verbose "decoded" (decode encoded)

smallcheckMany :: [Q (String, Exp)] -> ExpQ
smallcheckMany = doE . map (\f -> f >>= \(name, expr) -> noBindS [e| it name $ $(return expr) |])

verboseTrace :: Show a => Bool -> String -> a -> a
verboseTrace True msg x = trace (show (msg, x)) x
verboseTrace False _ x = x
