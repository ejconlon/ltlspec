{-# LANGUAGE TypeFamilies #-}

module Ltlspec.Recursion where

import Data.Functor.Foldable (Base, Recursive (..))

-- | Traverses a recursive structure bottom-up with the given effectful function.
-- 'Data.Functor.Foldable.fold' does this for a pure function of type `f a -> a`.
foldUpM :: (Recursive t, Base t ~ f, Traversable f, Monad m) => (f a -> m a) -> t -> m a
foldUpM h = go where
  go t = do
    let ft = project t
    fa <- traverse go ft
    h fa
