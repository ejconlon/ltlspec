-- | Linear Temporal Logic (LTL) propositions and functions for manipulating or evaluating them.
module Ltlspec where

import Control.Applicative (liftA2)
import Control.DeepSeq (NFData)
import Control.Monad.State.Strict (State, runState)
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

-- | An LTL proposition, with the recursion factored out.
-- For a proposition that looks like a tree, see 'Prop'.
-- For a proposition that looks like a graph, see 'GraphProp'.
-- Both use this datatype as the underlying shape.
-- For tree-shaped props, the 'r' holes are more tree-shaped props.
-- In both cases, the 'p' holes indexes predicates at each timestep
-- (that is to say they will be fed into a fresh predicate of type
-- 'p -> Bool' computed from the event that timestep).
data PropF r p =
    PropAtomF !p
  | PropTrueF
  | PropFalseF
  | PropNextF r
  | PropNotF r
  | PropUntilF r r  -- ^ 'PropUntil r1 r2' means 'eventually r2' and at least until 'r2' holds, 'r1' always holds.
                   -- If neither holds, it falsifies the proposition. When 'r2' holds, it satisfies the proposition.
  -- | PropReleaseF r r  -- ^ TODO
  | PropAlwaysF r
  | PropEventuallyF r
  | PropAndF [r]
  | PropOrF [r]
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (Hashable, NFData)

-- We can map over all the 'r' and 'p' holes in a 'PropF'.
instance Bifunctor PropF where
  bimap f g = \case
    PropAtomF p -> PropAtomF (g p)
    PropTrueF -> PropTrueF
    PropFalseF -> PropFalseF
    PropNextF r -> PropNextF (f r)
    PropNotF r -> PropNotF (f r)
    PropUntilF r1 r2 -> PropUntilF (f r1) (f r2)
    -- PropReleaseF r1 r2 -> PropReleaseF (f r1) (f r2)
    PropAlwaysF r -> PropAlwaysF (f r)
    PropEventuallyF r -> PropEventuallyF (f r)
    PropAndF rs -> PropAndF (fmap f rs)
    PropOrF rs -> PropOrF (fmap f rs)

-- We can fold over all the 'r' and 'p' holes in a 'PropF'.
instance Bifoldable PropF where
  bifoldr f g z = \case
    PropAtomF p -> g p z
    PropTrueF -> z
    PropFalseF -> z
    PropNextF r -> f r z
    PropNotF r -> f r z
    PropUntilF r1 r2 -> f r1 (f r2 z)
    -- PropReleaseF r1 r2 -> f r1 (f r2 z)
    PropAlwaysF r -> f r z
    PropEventuallyF r -> f r z
    PropAndF rs -> foldr f z rs
    PropOrF rs -> foldr f z rs

-- We can traverse over all the 'r' and 'p' holes in a 'PropF'.
instance Bitraversable PropF where
  bitraverse f g = \case
    PropAtomF p -> fmap PropAtomF (g p)
    PropTrueF -> pure PropTrueF
    PropFalseF -> pure PropFalseF
    PropNextF r -> fmap PropNextF (f r)
    PropNotF r -> fmap PropNotF (f r)
    PropUntilF r1 r2 -> liftA2 PropUntilF (f r1) (f r2)
    -- PropReleaseF r1 r2 -> liftA2 PropReleaseF (f r1) (f r2)
    PropAlwaysF r -> fmap PropAlwaysF (f r)
    PropEventuallyF r -> fmap PropEventuallyF (f r)
    PropAndF rs -> fmap PropAndF (traverse f rs)
    PropOrF rs -> fmap PropOrF (traverse f rs)

-- | An LTL proposition as a tree.
newtype Prop p = Prop { unProp :: PropF (Prop p) p }
  deriving newtype (Eq, Ord, Show, Hashable, NFData)

pattern PropAtom :: p -> Prop p
pattern PropAtom p = Prop (PropAtomF p)

pattern PropTrue :: Prop p
pattern PropTrue = Prop PropTrueF

pattern PropFalse :: Prop p
pattern PropFalse = Prop PropFalseF

pattern PropNext :: Prop p -> Prop p
pattern PropNext r = Prop (PropNextF r)

pattern PropNot :: Prop p -> Prop p
pattern PropNot r = Prop (PropNotF r)

pattern PropUntil :: Prop p -> Prop p -> Prop p
pattern PropUntil r1 r2 = Prop (PropUntilF r1 r2)

pattern PropAlways :: Prop p -> Prop p
pattern PropAlways r = Prop (PropAlwaysF r)

pattern PropEventually :: Prop p -> Prop p
pattern PropEventually r = Prop (PropEventuallyF r)

pattern PropAnd :: [Prop p] -> Prop p
pattern PropAnd rs = Prop (PropAndF rs)

pattern PropOr :: [Prop p] -> Prop p
pattern PropOr rs = Prop (PropOrF rs)

{-# COMPLETE PropAtom, PropTrue, PropFalse, PropNext, PropNot, PropUntil, PropAlways, PropEventually, PropAnd, PropOr #-}

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

propBottomUp :: (PropF x p -> x) -> Prop p -> x
propBottomUp f = onR where
  onR = f . onF . unProp
  onF = \case
    PropAtomF p -> PropAtomF p
    PropTrueF -> PropTrueF
    PropFalseF -> PropFalseF
    PropNextF r -> PropNextF (onR r)
    PropNotF r -> PropNotF (onR r)
    PropUntilF r1 r2 -> PropUntilF (onR r1) (onR r2)
    -- PropReleaseF r1 r2 -> undefined
    PropAlwaysF r -> PropAlwaysF (onR r)
    PropEventuallyF r -> PropEventuallyF (onR r)
    PropAndF rs -> PropAndF (fmap onR rs)
    PropOrF rs -> PropAndF (fmap onR rs)

-- | Find all the unique atoms in the proposition
propAtoms :: (Eq p, Hashable p) => Prop p -> HashSet p
propAtoms r0 = onR r0 HashSet.empty where
  onR r s = onF s (unProp r)
  onF = bifoldr onR HashSet.insert

-- | When we evaluate a proposition at a certain time step, we either
-- satisfy it, falsify it, or are left with another prop to evaluate
-- on the next timestep.
data PropRes p =
    PropResTrue
  | PropResFalse
  | PropResNext !(Prop p)
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (Hashable, NFData)

propResNot :: PropRes p -> PropRes p
propResNot = \case
  PropResTrue -> PropResFalse
  PropResFalse -> PropResTrue
  PropResNext r -> PropResNext (Prop (PropNotF r))

-- | Evaluate the proposition at the current timestep with the given evaluation function.
-- (See also evalGraph.)
evalProp :: (p -> Bool) -> Prop p -> PropRes p
evalProp f = go where
  go p0@(Prop f0) =
    case f0 of
      PropAtomF p -> if f p then PropResTrue else PropResFalse
      PropTrueF -> PropResTrue
      PropFalseF -> PropResFalse
      PropNextF r -> PropResNext r
      PropNotF r -> propResNot (go r)
      -- "Until" logic from "Runtime Verification of Concurrent Haskell Programs" s3.2, p7.
      PropUntilF r1 r2 ->
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
              PropResNext r1' -> PropResNext (Prop (PropAndF [r1', p0]))
          -- If r2 advances,
          PropResNext r2' ->
            case go r1 of
              -- If r1 does not hold, then we are absolved of the until only if r2' holds
              PropResFalse -> PropResNext r2'
              -- If r1 does hold, then we can either wait for absolution or keep on with the until
              PropResTrue -> PropResNext (Prop (PropOrF [r2', p0]))
              -- If r1 advances, we follow similar logic
              PropResNext r1' -> PropResNext (Prop (PropOrF [r2', Prop (PropAndF [r1', p0])]))
      -- PropRelease _ _ -> error "TODO"
      PropAlwaysF r ->
        case go r of
          PropResTrue -> PropResNext p0
          PropResFalse -> PropResFalse
          PropResNext r' -> PropResNext (Prop (PropAndF [r', p0]))
      PropEventuallyF r ->
        case go r of
          PropResTrue -> PropResTrue
          PropResFalse -> PropResNext p0
          PropResNext r' -> PropResNext (Prop (PropOrF [r', p0]))
      PropAndF rs -> foldAnds [] rs
      PropOrF rs -> foldOrs [] rs
  foldAnds acc = \case
    [] ->
      case acc of
        [] -> PropResTrue
        _ -> PropResNext (Prop (PropAndF (reverse acc)))
    x:xs ->
      case go x of
        PropResTrue -> foldAnds acc xs
        PropResFalse -> PropResFalse
        PropResNext r -> foldAnds (r:acc) xs
  foldOrs acc = \case
    [] ->
      case acc of
        [] -> PropResFalse
        _ -> PropResNext (Prop (PropOrF (reverse acc)))
    x:xs ->
      case go x of
        PropResTrue -> PropResTrue
        PropResFalse -> foldOrs acc xs
        PropResNext r -> foldOrs (r:acc) xs

-- | Evaluate the prop at every timestep until true/false or there are no more inputs.
-- Also returns the number of timesteps evaluated.
foldProp :: (a -> p -> Bool) -> Prop p -> [a] -> (Int, PropRes p)
foldProp f = go 0 where
  go i p xs =
    case xs of
      [] -> (i, PropResNext p)
      y:ys ->
        let i' = i + 1
            r = evalProp (f y) p
        in case r of
          PropResNext p' -> go i' p' ys
          _ -> (i', r)

-- | We label each node in our graph with a 'GraphId'.
newtype GraphId = GraphId { unGraphId :: Int }
  deriving newtype (Eq, Ord, Show, Num, Enum, Bounded, Hashable, NFData)

-- | A restricted proposition that has no recursive structures - it can
-- only refer to other graph nodes. (See 'Graph' the whole structure.)
newtype GraphProp p = GraphProp { unGraphProp :: PropF GraphId p }
  deriving newtype (Eq, Ord, Show, Functor, Foldable, Hashable, NFData)
  deriving stock (Traversable)

-- | Map that maintains unique key-value pairs
data UniqueMap k v = UniqueMap
  { uniqueMapFwd :: !(HashMap k v)
  , uniqueMapBwd :: !(HashMap v k)
  , uniqueMapSrc :: !k
  } deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData)

emptyUniqueMap :: Enum k => UniqueMap k v
emptyUniqueMap = UniqueMap HashMap.empty HashMap.empty (toEnum 0)

uniqueMapInsert :: (Enum k, Eq k, Hashable k, Eq v, Hashable v) => v -> UniqueMap k v -> (k, UniqueMap k v)
uniqueMapInsert val umap@(UniqueMap fwd bwd src) =
  case HashMap.lookup val bwd of
    Just key -> (key, umap)
    Nothing ->
      let fwd' = HashMap.insert src val fwd
          bwd' = HashMap.insert val src bwd
          src' = succ src
      in (src, UniqueMap fwd' bwd' src')

uniqueMapLookup :: (Eq k, Hashable k) => k -> UniqueMap k v -> Maybe v
uniqueMapLookup k = HashMap.lookup k . uniqueMapFwd

-- | Garbage collect from roots
uniqueMapCollect :: (Eq k, Hashable k, Eq v, Hashable v) => (v -> [k]) -> [k] -> (HashSet k, UniqueMap k v)
uniqueMapCollect = go HashSet.empty where
  go = error "TODO"

-- | Intermediate state for building a 'Graph'.
-- TODO put in a map the other way of GraphProp to GraphId
data GraphState p = GraphState
  { graphStateNodes :: !(UniqueMap GraphId (GraphProp p))
  , graphStateNext :: !GraphId
  } deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData)

-- | Initial state for building a 'Graph'.
emptyGraphState :: GraphState p
emptyGraphState = GraphState emptyUniqueMap 0

-- | An LTL proposition as a graph. Note that if it isn't a DAG
-- you're going to have a very bad time evaluating it!
data Graph p = Graph
  { graphState :: !(GraphState p)
  , graphRoot :: !GraphId
  } deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData)

propToGraphStep :: Prop p -> State (GraphState p) GraphId
propToGraphStep = error "TODO"

-- | Turn the given tree-structured proposition into a graph.
-- Should satisfy `evalGraph f (propToGraph p) == fmap propToGraph (evalProp f p)`.
-- That is to say, for propositions that are naturally tree-structured, we
-- get the same result evaluating them in tree or graph form.
propToGraph :: Prop p -> Graph p
propToGraph p =
  let (root, st) = runState (propToGraphStep p) emptyGraphState
  in Graph st root

-- | Evaluate the proposition at the current timestep with the given evaluation function.
-- (See also 'evalProp'.)
evalGraph :: (p -> Bool) -> Graph p -> Either Bool (Graph p)
evalGraph = error "TODO"
