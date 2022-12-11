module PeekyBlinders.FoldableExtras where

import PeekyBlinders.Prelude

{-# INLINE consumeMonadically #-}
consumeMonadically :: (Foldable f, Monad m) => f a -> b -> (b -> a -> m b) -> m b
consumeMonadically foldable init step =
  foldM step init foldable
