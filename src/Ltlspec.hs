module Ltlspec where

import Control.Applicative (liftA2)
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as Map

data PropF r p =
    PropAtom !p
  | PropTrue
  | PropFalse
  | PropNext r
  | PropNot r
  | PropUntil r r
  | PropRelease r r
  | PropAnd [r]
  | PropOr [r]
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Bifunctor PropF where
  bimap f g = \case
    PropAtom p -> PropAtom (g p)
    PropTrue -> PropTrue
    PropFalse -> PropFalse
    PropNext r -> PropNext (f r)
    PropNot r -> PropNot (f r)
    PropUntil r1 r2 -> PropUntil (f r1) (f r2)
    PropRelease r1 r2 -> PropRelease (f r1) (f r2)
    PropAnd rs -> PropAnd (fmap f rs)
    PropOr rs -> PropOr (fmap f rs)

instance Bifoldable PropF where
  bifoldr f g z = \case
    PropAtom p -> g p z
    PropTrue -> z
    PropFalse -> z
    PropNext r -> f r z
    PropNot r -> f r z
    PropUntil r1 r2 -> f r1 (f r2 z)
    PropRelease r1 r2 -> f r1 (f r2 z)
    PropAnd rs -> foldr f z rs
    PropOr rs -> foldr f z rs

instance Bitraversable PropF where
  bitraverse f g = \case
    PropAtom p -> fmap PropAtom (g p)
    PropTrue -> pure PropTrue
    PropFalse -> pure PropFalse
    PropNext r -> fmap PropNext (f r)
    PropNot r -> fmap PropNot (f r)
    PropUntil r1 r2 -> liftA2 PropUntil (f r1) (f r2)
    PropRelease r1 r2 -> liftA2 PropRelease (f r1) (f r2)
    PropAnd rs -> fmap PropAnd (traverse f rs)
    PropOr rs -> fmap PropOr (traverse f rs)

newtype Prop p = Prop { unProp :: PropF (Prop p) p }
  deriving stock (Eq, Ord, Show)

instance Functor Prop where
  fmap f = onR where
    onR = Prop . onF . unProp
    onF = bimap onR f

instance Foldable Prop where
  foldr f z0 r0 = onR r0 z0 where
    onR = onF . unProp
    onF x1 z1 = bifoldr onR f z1 x1

instance Traversable Prop where
  traverse f = onR where
    onR = fmap Prop . onF . unProp
    onF = bitraverse onR f

evalProp :: (p -> Bool) -> Prop p -> Either Bool (Prop p)
evalProp f = onR where
  onR = fmap Prop . onF . unProp
  onF = \case
    PropAtom p -> Left (f p)
    PropTrue -> Left True
    PropFalse -> Left False
    PropNext r -> Right (unProp r)
    PropNot r -> either (Left . not) (Right . PropNot) (onR r)
    PropUntil r1 r2 ->
      let e1 = onR r1
      in case e1 of
        Left True -> Right (unProp r2)
        Left False -> Left False
        Right r1' -> Right (PropUntil r1' r2)
    PropRelease r1 r2 -> undefined
    PropAnd rs -> undefined
    PropOr rs -> undefined

newtype GraphId = GraphId { unGraphId :: Int }
  deriving newtype (Eq, Ord, Show, Num, Enum, Bounded)

data GraphPropF r p =
    GraphPropRef !GraphId
  | GraphPropForm !(PropF r p)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Bifunctor GraphPropF where
  bimap f g = \case
    GraphPropRef i -> GraphPropRef i
    GraphPropForm j -> GraphPropForm (bimap f g j)

instance Bifoldable GraphPropF where
  bifoldr f g z = \case
    GraphPropRef _ -> z
    GraphPropForm j -> bifoldr f g z j

instance Bitraversable GraphPropF where
  bitraverse f g = \case
    GraphPropRef i -> pure (GraphPropRef i)
    GraphPropForm j -> fmap GraphPropForm (bitraverse f g j)

newtype GraphProp p = GraphProp { unGraphProp :: GraphPropF (GraphProp p) p }
  deriving stock (Eq, Ord, Show)

instance Functor GraphProp where
  fmap f = onR where
    onR = GraphProp . onF . unGraphProp
    onF = bimap onR f

instance Foldable GraphProp where
  foldr f z0 r0 = onR r0 z0 where
    onR = onF . unGraphProp
    onF x1 z1 = bifoldr onR f z1 x1

instance Traversable GraphProp where
  traverse f = onR where
    onR = fmap GraphProp . onF . unGraphProp
    onF = bitraverse onR f

data Graph p = Graph
  { graphNodes :: !(Map GraphId (GraphProp p))
  , graphRoot :: !GraphId
  } deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

evalGraph :: (p -> Bool) -> Graph p -> Either Bool (Graph p)
evalGraph = undefined
