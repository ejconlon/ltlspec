module Ltlspec where

import Control.Applicative (liftA2)
import Control.Monad.State (State, runState)
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- | An LTL proposition, with the recursion factored out.
-- For a proposition that looks like a tree, see 'Prop'.
data PropF r p =
    PropAtom !p
  | PropTrue
  | PropFalse
  | PropNext r
  | PropNot r
  | PropUntil r r  -- ^ 'PropUntil r1 r2' means 'eventually r2' and at least until 'r2' holds, 'r1' always holds.
                   -- If neither holds, it falsifies the proposition. When 'r2' holds, it satisfies the proposition.
  -- | PropRelease r r
  | PropAlways r
  | PropEventually r
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
    -- PropRelease r1 r2 -> PropRelease (f r1) (f r2)
    PropAlways r -> PropAlways (f r)
    PropEventually r -> PropEventually (f r)
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
    -- PropRelease r1 r2 -> f r1 (f r2 z)
    PropAlways r -> f r z
    PropEventually r -> f r z
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
    -- PropRelease r1 r2 -> liftA2 PropRelease (f r1) (f r2)
    PropAlways r -> fmap PropAlways (f r)
    PropEventually r -> fmap PropEventually (f r)
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

data PropRes p =
    PropResTrue
  | PropResFalse
  | PropResNext !(Prop p)
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

propResNot :: PropRes p -> PropRes p
propResNot = \case
  PropResTrue -> PropResFalse
  PropResFalse -> PropResTrue
  PropResNext r -> PropResNext (Prop (PropNot r))

evalProp :: (p -> Bool) -> Prop p -> PropRes p
evalProp f = go where
  go p0@(Prop f0) =
    case f0 of
      PropAtom p -> if f p then PropResTrue else PropResFalse
      PropTrue -> PropResTrue
      PropFalse -> PropResFalse
      PropNext r -> PropResNext r
      PropNot r -> propResNot (go r)
      -- "Until" logic from "Runtime Verification of Concurrent Haskell Programs" s3.2, p7.
      PropUntil r1 r2 ->
        case go r2 of
          -- Once r2 holds, the proposition is satisfied
          PropResTrue -> PropResTrue
          -- If r2 does not hold,
          PropResFalse ->
            case go r1 of
              -- If r1 does not hold, the proposition is falsified
              PropResFalse -> PropResFalse
              -- If r1 holds, we are still in the until
              PropResTrue -> PropResNext p0
              -- If r1 advances, we need to satisfy the new prop and the existing until prop
              PropResNext r1' -> PropResNext (Prop (PropAnd [r1', p0]))
          -- If r2 advances,
          PropResNext r2' ->
            case go r1 of
              -- If r1 does not hold, then we are absolved of the until only if r2' holds
              PropResFalse -> PropResNext r2'
              -- If r1 does hold, then we can either wait for absolution or keep on with the until
              PropResTrue -> PropResNext (Prop (PropOr [r2', p0]))
              -- If r1 advances, we follow similar logic
              PropResNext r1' -> PropResNext (Prop (PropOr [r2', Prop (PropAnd [r1', p0])]))
      -- PropRelease _ _ -> error "TODO"
      PropAlways r ->
        case go r of
          PropResTrue -> PropResNext p0
          PropResFalse -> PropResFalse
          PropResNext r' -> PropResNext (Prop (PropAnd [r', p0]))
      PropEventually r ->
        case go r of
          PropResTrue -> PropResTrue
          PropResFalse -> PropResNext p0
          PropResNext r' -> PropResNext (Prop (PropOr [r', p0]))
      PropAnd rs -> foldAnds [] rs
      PropOr rs -> foldOrs [] rs
  foldAnds acc = \case
    [] ->
      case acc of
        [] -> PropResTrue
        _ -> PropResNext (Prop (PropAnd (reverse acc)))
    x:xs ->
      case go x of
        PropResTrue -> foldAnds acc xs
        PropResFalse -> PropResFalse
        PropResNext r -> foldAnds (r:acc) xs
  foldOrs acc = \case
    [] ->
      case acc of
        [] -> PropResFalse
        _ -> PropResNext (Prop (PropOr (reverse acc)))
    x:xs ->
      case go x of
        PropResTrue -> PropResTrue
        PropResFalse -> foldOrs acc xs
        PropResNext r -> foldOrs (r:acc) xs

newtype GraphId = GraphId { unGraphId :: Int }
  deriving newtype (Eq, Ord, Show, Num, Enum, Bounded)

newtype GraphProp p = GraphProp { unGraphProp :: PropF GraphId p }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

data GraphState p = GraphState
  { graphStateNodes :: !(Map GraphId (GraphProp p))
  , graphStateNext :: !GraphId
  } deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

emptyGraphState :: GraphState p
emptyGraphState = GraphState Map.empty 0

data Graph p = Graph
  { graphState :: !(GraphState p)
  , graphRoot :: !GraphId
  } deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

propToGraphStep :: Prop p -> State (GraphState p) GraphId
propToGraphStep = undefined

propToGraph :: Prop p -> Graph p
propToGraph p =
  let (root, st) = runState (propToGraphStep p) emptyGraphState
  in Graph st root

evalGraph :: (p -> Bool) -> Graph p -> Either Bool (Graph p)
evalGraph = undefined
