module Ltlspec.Recursion where

import Data.Functor.Foldable (Base, Recursive (..))

-- | Traverses a recursive structure
foldUpM :: (Recursive t, Base t ~ f, Traversable f, Monad m) => (f a -> m a) -> t -> m a
foldUpM h = go where
  go t = do
    let ft = project t
    fa <- traverse go ft
    h fa
