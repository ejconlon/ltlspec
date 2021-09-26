-- | Linear Temporal Logic (LTL) propositions and functions for manipulating and evaluating them.
--
-- These definitions may seem a little strange: Our representation of LTL propositions, 'Prop',
-- is defined as a fixpoint of another type, 'PropF' (mnemonic 'F' for 'Functor'; in this case, a
-- structure with holes). This 'PropF' has holes for the recursive parts of 'Prop' and holes for the
-- leaf atoms. We define 'Bifunctor', 'Bifoldable', and 'Bitraversable' for this type so we can
-- easily modify and consume the contents of these holes. Why go through the trouble in the first
-- place? The payoff is being able to use 'PropF' later on in the graph representation 'Graph' so
-- we can exploit structural sharing and avoid size blowup as we evaluate the proposition.
--
-- To reduce noise at definition sites, we define pattern synonyms for all proposition constructors.
-- For example, instead of 'Prop (PropNotF (Prop PropFalseF))' you can write 'PropNot PropFalse'.
-- It is a little clumsy to import them ('import LtlSpec (pattern PropNot, pattern PropFalse))'), but
-- that's how it goes.
module Ltlspec where

import Control.Applicative (liftA2)
import Control.DeepSeq (NFData)
import Control.Monad (ap, (>=>))
import Control.Monad.Except (Except, MonadError, runExcept, throwError)
import Control.Monad.State.Strict (MonadState, State, StateT, evalState, get, runState, runStateT, state)
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import Data.Monoid (Sum (..))
import Data.Semigroup (Max (..))
import GHC.Generics (Generic)

-- | An LTL proposition, with the recursion factored out.
-- For a proposition that looks like a tree, see 'Prop'.
-- For a proposition that looks like a graph, see 'GraphProp'.
-- Both use this datatype as the underlying shape.
-- For tree-shaped props, the 'r' holes are more tree-shaped props.
-- For graph-shaped props, the 'r' holes are node ids.
-- In both cases, the 'p' holes are atoms.
--
-- TODO(ejconlon) Remove all ctors not necessary for Negation Normal Form
-- (always, eventually), add helpers to construct equivalents, and
-- add function to normalize. Required for translation to Buchi automaton.
data PropF r p =
    PropAtomF !p
  -- ^ An atomic prop - use this to embed predicates from your domain
  | PropTrueF
  -- ^ The constrant True
  | PropFalseF
  -- ^ The constant False
  | PropNotF r
  -- ^ Logical negation of the prop
  | PropAndF [r]
  -- ^ Logical AND of several props (empty is true)
  | PropOrF [r]
  -- ^ Logical OR of several props (empty is false)
  | PropNextF r
  -- ^ A prop that holds the next timestamp
  | PropUntilF r r
  -- ^ 'PropUntil r1 r2' means 'eventually r2' and at least until 'r2' holds, 'r1' always holds.
  -- If both are false, the prop is false. When 'r2' holds, the prop is true.
  | PropReleaseF r r
  -- ^ 'PropRelease r1 r2' means 'always r2' until and including when 'r1' holds.
  -- If 'r2' is false, the prop is false. When 'r1' and 'r2' hold, the prop is true.
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (Hashable, NFData)

-- | We can map over all the 'r' and 'p' holes in a 'PropF'.
instance Bifunctor PropF where
  bimap f g = \case
    PropAtomF p -> PropAtomF (g p)
    PropTrueF -> PropTrueF
    PropFalseF -> PropFalseF
    PropNotF r -> PropNotF (f r)
    PropAndF rs -> PropAndF (fmap f rs)
    PropOrF rs -> PropOrF (fmap f rs)
    PropNextF r -> PropNextF (f r)
    PropUntilF r1 r2 -> PropUntilF (f r1) (f r2)
    PropReleaseF r1 r2 -> PropReleaseF (f r1) (f r2)

-- | We can fold over all the 'r' and 'p' holes in a 'PropF'.
instance Bifoldable PropF where
  bifoldr f g z = \case
    PropAtomF p -> g p z
    PropTrueF -> z
    PropFalseF -> z
    PropNotF r -> f r z
    PropAndF rs -> foldr f z rs
    PropOrF rs -> foldr f z rs
    PropNextF r -> f r z
    PropUntilF r1 r2 -> f r1 (f r2 z)
    PropReleaseF r1 r2 -> f r1 (f r2 z)

-- | We can traverse over all the 'r' and 'p' holes in a 'PropF'.
instance Bitraversable PropF where
  bitraverse f g = \case
    PropAtomF p -> fmap PropAtomF (g p)
    PropTrueF -> pure PropTrueF
    PropFalseF -> pure PropFalseF
    PropNotF r -> fmap PropNotF (f r)
    PropAndF rs -> fmap PropAndF (traverse f rs)
    PropOrF rs -> fmap PropOrF (traverse f rs)
    PropNextF r -> fmap PropNextF (f r)
    PropUntilF r1 r2 -> liftA2 PropUntilF (f r1) (f r2)
    PropReleaseF r1 r2 -> liftA2 PropReleaseF (f r1) (f r2)

-- | An LTL proposition as a tree.
newtype Prop p = Prop { unProp :: PropF (Prop p) p }
  deriving newtype (Eq, Ord, Show, Hashable, NFData)

pattern PropAtom :: p -> Prop p
pattern PropAtom p = Prop (PropAtomF p)

pattern PropTrue :: Prop p
pattern PropTrue = Prop PropTrueF

pattern PropFalse :: Prop p
pattern PropFalse = Prop PropFalseF

pattern PropNot :: Prop p -> Prop p
pattern PropNot r = Prop (PropNotF r)

pattern PropAnd :: [Prop p] -> Prop p
pattern PropAnd rs = Prop (PropAndF rs)

pattern PropOr :: [Prop p] -> Prop p
pattern PropOr rs = Prop (PropOrF rs)

pattern PropNext :: Prop p -> Prop p
pattern PropNext r = Prop (PropNextF r)

pattern PropUntil :: Prop p -> Prop p -> Prop p
pattern PropUntil r1 r2 = Prop (PropUntilF r1 r2)

pattern PropRelease :: Prop p -> Prop p -> Prop p
pattern PropRelease r1 r2 = Prop (PropReleaseF r1 r2)

{-# COMPLETE PropAtom, PropTrue, PropFalse, PropNot, PropAnd, PropOr, PropNext, PropUntil, PropRelease #-}

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

instance Applicative Prop where
  pure = PropAtom
  (<*>) = ap

instance Monad Prop where
  return = pure
  p >>= f = propBind f p

-- | Substitution of atoms in a 'Prop' is a monadic bind.
--
-- >>> propBind (pure . (+1)) (PropNot (PropAtom 1))
-- PropNotF (PropAtomF 2)
--
propBind :: (p -> Prop q) -> Prop p -> Prop q
propBind f = onR where
  onR = Prop . onF . unProp
  onF = \case
    PropAtomF p -> unProp (f p)
    PropTrueF -> PropTrueF
    PropFalseF -> PropFalseF
    PropNotF r -> PropNotF (onR r)
    PropAndF rs -> PropAndF (fmap onR rs)
    PropOrF rs -> PropAndF (fmap onR rs)
    PropNextF r -> PropNextF (onR r)
    PropUntilF r1 r2 -> PropUntilF (onR r1) (onR r2)
    PropReleaseF r1 r2 -> PropReleaseF (onR r1) (onR r2)

-- | Fold a 'Prop' from the bottom up.
--
-- >>> propFoldUp (bifoldr (+) (+) 0) (PropUntil (PropAtom 1) (PropAtom 2))
-- 3
--
propFoldUp :: (PropF x p -> x) -> Prop p -> x
propFoldUp f = onR where
  onR = f . onF . unProp
  onF = first onR

-- Fold a 'Prop' from the bottom up, with effects.
propFoldUpM :: Monad m => (PropF x p -> m x) -> Prop p -> m x
propFoldUpM f = onR where
  onR = onF . unProp >=> f
  onF = \case
    PropAtomF p -> pure (PropAtomF p)
    PropTrueF -> pure PropTrueF
    PropFalseF -> pure PropFalseF
    PropNotF r -> fmap PropNotF (onR r)
    PropAndF rs -> fmap PropAndF (traverse onR rs)
    PropOrF rs -> fmap PropAndF (traverse onR rs)
    PropNextF r -> fmap PropNextF (onR r)
    PropUntilF r1 r2 -> liftA2 PropUntilF (onR r1) (onR r2)
    PropReleaseF r1 r2 -> liftA2 PropReleaseF (onR r1) (onR r2)

-- | A prop that holds at every timestep. If it is ever false, the prop is false.
propAlways :: Prop p -> Prop p
propAlways = PropRelease PropFalse

-- | A prop that will hold at some timestep. If it is ever true, the prop is true.
propEventually :: Prop p -> Prop p
propEventually = PropUntil PropTrue

-- | Propositional implication: r1 -> r2
propIf :: Prop p -> Prop p -> Prop p
propIf r1 r2 = PropOr [PropNot r1, r2]

-- | Bidiriectional propositional implication: r1 <-> r2
propIff :: Prop p -> Prop p -> Prop p
propIff r1 r2 = PropAnd [propIf r1 r2, propIf r2 r1]

-- | The size of the 'Prop' (number of constructors)
propSize :: Prop a -> Int
propSize = getSum . propFoldUp ((Sum 1 <>) . bifoldMap id mempty)

-- | The depth of the 'Prop' (max length from root to leaf)
propDepth :: Prop a -> Int
propDepth = getMax . propFoldUp (succ . bifoldMap id mempty)

-- | Gathers all the unique atoms in the proposition
--
-- >>> propAtoms (PropUntil (PropAtom 1) (PropAtom 2))
-- fromList [1,2]
--
propAtoms :: (Eq p, Hashable p) => Prop p -> HashSet p
propAtoms r0 = onR r0 HashSet.empty where
  onR r s = onF s (unProp r)
  onF = bifoldr onR HashSet.insert

-- | When we evaluate a proposition at a certain time step, we either
-- satisfy it, falsify it, or are left with another prop to evaluate
-- on the next timestep.
data PropRes x =
    PropResTrue
  | PropResFalse
  | PropResNext !x
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (Hashable, NFData)

-- | Negates the result
propResNot :: PropRes (Prop p) -> PropRes (Prop p)
propResNot = \case
  PropResTrue -> PropResFalse
  PropResFalse -> PropResTrue
  PropResNext r -> PropResNext (Prop (PropNotF r))

-- | Evaluate the proposition at the current timestep with the given evaluation function.
-- (See also graphEval.)
propEval :: (p -> Bool) -> Prop p -> PropRes (Prop p)
propEval f = go where
  go p0@(Prop f0) =
    case f0 of
      PropAtomF p -> if f p then PropResTrue else PropResFalse
      PropTrueF -> PropResTrue
      PropFalseF -> PropResFalse
      PropNotF r -> propResNot (go r)
      PropAndF rs -> foldAnds [] rs
      PropOrF rs -> foldOrs [] rs
      PropNextF r -> PropResNext r
      -- See "Until" logic from "Runtime Verification of Concurrent Haskell Programs" s3.2, p7.
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
      PropReleaseF r1 r2 ->
        case go r2 of
          -- If r2 does not hold, the proposition is falsified
          PropResFalse -> PropResFalse
          -- If r2 holds,
          PropResTrue ->
            case go r1 of
              -- If r1 does not hold, we are still in the release
              PropResFalse -> PropResNext p0
              -- If r1 holds, the proposition is satisfied
              PropResTrue -> PropResTrue
              -- If r1 advances, we need to satisfy the new prop and the existing release prop
              PropResNext _ -> error "TODO" -- PropResNext (Prop (PropAndF [r1', p0]))
          -- If r2 advances,
          PropResNext _ ->
            case go r1 of
              PropResFalse -> error "TODO"
              PropResTrue -> error "TODO"
              PropResNext _ -> error "TODO"
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
propFold :: (a -> p -> Bool) -> Prop p -> [a] -> (Int, PropRes (Prop p))
propFold f = go 0 where
  go i p xs =
    case xs of
      [] -> (i, PropResNext p)
      y:ys ->
        let i' = i + 1
            r = propEval (f y) p
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

-- | Get the immediate child graph ids of the given 'GraphProp'.
--
-- >>> graphPropChildren (GraphProp (PropUntilF 2 3))
-- [2,3]
--
graphPropChildren :: GraphProp p -> [GraphId]
graphPropChildren = bifoldr (:) (const id) [] . unGraphProp

-- | Map that maintains unique key-value pairs
data UniqueMap k v = UniqueMap
  { uniqueMapFwd :: !(HashMap k v)
  , uniqueMapBwd :: !(HashMap v k)
  , uniqueMapSrc :: !k
  } deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData)

uniqueMapSize :: UniqueMap k v -> Int
uniqueMapSize = HashMap.size . uniqueMapFwd

emptyUniqueMap :: Enum k => UniqueMap k v
emptyUniqueMap = UniqueMap HashMap.empty HashMap.empty (toEnum 0)

-- | Insert a value into the map, using the next key.
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

uniqueMapReachable :: (Eq k, Hashable k) => (v -> [k]) -> [k] -> UniqueMap k v -> HashSet k
uniqueMapReachable f ks0 um = go HashSet.empty ks0 where
  go !seen xs =
    case xs of
      [] -> seen
      k:ks ->
        if HashSet.member k seen
          then go seen ks
          else case HashMap.lookup k (uniqueMapFwd um) of
            Nothing -> go seen ks
            Just v ->
              let ks' = [k' | k' <- f v, not (HashSet.member k' seen)]
              in go (HashSet.insert k seen) (ks' ++ ks)

uniqueMapFilterWithKey :: (Eq k, Hashable k, Eq v, Hashable v) => (k -> v -> Bool) -> UniqueMap k v -> UniqueMap k v
uniqueMapFilterWithKey f (UniqueMap fwd0 bwd0 src0) = go fwd0 bwd0 (HashMap.toList fwd0) where
  go !fwd !bwd xs =
    case xs of
      [] -> UniqueMap fwd bwd src0
      ((k, v):kvs) ->
        if f k v
          then go fwd bwd kvs
          else go (HashMap.delete k fwd) (HashMap.delete v bwd) kvs

-- | Garbage collect from root
uniqueMapCollect :: (Eq k, Hashable k, Eq v, Hashable v) => (v -> [k]) -> [k] -> UniqueMap k v -> UniqueMap k v
uniqueMapCollect f roots um0 =
  let reachable = uniqueMapReachable f roots um0
  in uniqueMapFilterWithKey (\k _ -> HashSet.member k reachable) um0

type GraphState p = UniqueMap GraphId (GraphProp p)

-- | Initial state for building a 'Graph'.
emptyGraphState :: GraphState p
emptyGraphState = emptyUniqueMap

-- | Error for when we try to reference a missing node.
-- If this is ever thrown we implemented something wrong.
newtype GraphIdMissing = GraphIdMissing { unGraphIdMissing :: GraphId }
  deriving newtype (Eq, Ord, Show, Hashable)

-- | A simple set of effects for graph traversal.
newtype GSM p a = GSM { unGSM :: StateT (GraphState p) (Except GraphIdMissing) a }
  deriving newtype (Functor, Applicative, Monad, MonadState (GraphState p), MonadError GraphIdMissing)

runGSM :: GSM p a -> GraphState p -> Either GraphIdMissing (a, GraphState p)
runGSM (GSM m) = runExcept . runStateT m

stateToGSM :: State (GraphState p) a -> GSM p a
stateToGSM = state . runState

stateFromGSM :: GSM p a -> State (GraphState p) (Either GraphIdMissing a)
stateFromGSM m = state $ \s ->
  case runGSM m s of
    Left e -> (Left e, s)
    Right (a, s') -> (Right a, s')

-- | An LTL proposition as a graph. Note that if it isn't a DAG
-- you're going to have a very bad time evaluating it!
data Graph p = Graph
  { graphState :: !(GraphState p)
  , graphRoot :: !GraphId
  } deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData)

graphInsert :: (Eq p, Hashable p) => GraphProp p -> State (GraphState p) GraphId
graphInsert = state . uniqueMapInsert

propToGraphStep :: (Eq p, Hashable p) => Prop p -> State (GraphState p) GraphId
propToGraphStep = propFoldUpM (graphInsert . GraphProp)

-- | Turn the given tree-structured proposition into a graph.
-- Should satisfy `evalGraph f (propToGraph p) == fmap propToGraph (evalProp f p)`.
-- That is to say, for propositions that are naturally tree-structured, we
-- get the same result evaluating them in tree or graph form.
propToGraph :: (Eq p, Hashable p) => Prop p -> Graph p
propToGraph p =
  let (root, st) = runState (propToGraphStep p) emptyGraphState
  in Graph st root

graphResEnrich :: GraphState p -> PropRes GraphId -> PropRes (Graph p)
graphResEnrich = fmap . Graph

-- | Negates the result
graphResNot :: (Eq p, Hashable p) => PropRes GraphId -> State (GraphState p) (PropRes GraphId)
graphResNot = \case
  PropResTrue -> pure PropResFalse
  PropResFalse -> pure PropResTrue
  PropResNext i -> fmap PropResNext (graphInsert (GraphProp (PropNotF i)))

graphEvalStep :: (Eq p, Hashable p) => (p -> Bool) -> GraphId -> GSM p (PropRes GraphId)
graphEvalStep f = go where
  go id0 = do
    st <- get
    case uniqueMapLookup id0 st of
      Nothing -> throwError (GraphIdMissing id0)
      Just p0 ->
        case unGraphProp p0 of
          PropAtomF p -> pure (if f p then PropResTrue else PropResFalse)
          PropTrueF -> pure PropResTrue
          PropFalseF -> pure PropResFalse
          PropNotF r -> do
            res <- go r
            stateToGSM (graphResNot res)
          PropNextF r -> pure (PropResNext r)
          _ -> error "TODO"

-- | Evaluate the proposition at the current timestep with the given evaluation function.
-- (See also 'propEval'.)
graphEval :: (Eq p, Hashable p) => (p -> Bool) -> Graph p -> Either GraphIdMissing (PropRes (Graph p))
graphEval f (Graph st root) =
  let ea = runGSM (graphEvalStep f root) st
  in fmap (\(res, st') -> graphResEnrich st' res) ea

graphFold :: (Eq p, Hashable p) => (a -> p -> Bool) -> Graph p -> [a] -> (Int, Either GraphIdMissing (PropRes (Graph p)))
graphFold f g@(Graph st0 root) zs = evalState (go 0 root zs) st0 where
  go c i xs =
    case xs of
      [] -> pure (c, Right (PropResNext g))
      y:ys -> do
        let c' = c + 1
        eres <- stateFromGSM (graphEvalStep (f y) i)
        case eres of
          Left e -> pure (c', Left e)
          Right res ->
            case res of
              PropResNext i' -> go c' i' ys
              _ -> do
                st <- get
                pure (c', Right (graphResEnrich st res))

-- | Count of nodes reachable from the root of the graph.
graphReachableSize :: Graph p -> Int
graphReachableSize g =
  let um = graphState g
      reachable = uniqueMapReachable graphPropChildren [graphRoot g] um
  in HashSet.size reachable

-- | Count of all nodes in the graph.
graphTotalSize :: Graph p -> Int
graphTotalSize = HashMap.size . uniqueMapFwd . graphState

-- graphReachableDepth :: Graph p -> Int
-- grpahReachableDepth = error "TODO"

-- | Fold a 'Graph' from the bottom up.
-- graphFoldUp :: (PropF x p -> x) -> Graph p -> x
-- graphFoldUp f = error "TODO"

graphCollect :: (Eq p, Hashable p) => Graph p -> Graph p
graphCollect g =
  let um = graphState g
      um' = uniqueMapCollect graphPropChildren [graphRoot g] um
  in g { graphState = um' }
