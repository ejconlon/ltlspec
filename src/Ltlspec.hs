{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | Linear Temporal Logic (LTL) propositions and functions for manipulating and evaluating them.
module Ltlspec where

import Control.DeepSeq (NFData)
import Control.Monad.Writer.Strict (execWriter, tell)
import Data.Functor.Foldable (embed, fold, project)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import Data.Semigroup (Max (..), Sum (..))
import GHC.Generics (Generic)
import Ltlspec.Recursion (foldUpM)

type PropName = String
type TyName = String
type AxiomName = String

type TyDefs = [TyName]
type PropDefs = Map PropName [TyName]
type AxiomDefs = Map AxiomName Prop

data Theory = Theory
  { theoryTypes :: !TyDefs
  , theoryProps :: !PropDefs
  , theoryAxioms :: !AxiomDefs
  } deriving stock (Eq, Show)

type VarName = String

data Atom = Atom !PropName ![VarName]
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

data Binder = Binder !VarName !TyName
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

-- | An LTL proposition with first-order data quantification.
-- This selection of operators corresponds to "Release Positive Normal Form"
data Prop =
    PropAtom !Atom
  -- ^ An atomic prop - use this to embed predicates from your domain
  | PropTrue
  -- ^ The constrant True
  | PropFalse
  -- ^ The constant False
  | PropNot Prop
  -- ^ Logical negation of the prop
  | PropAnd Prop Prop
  -- ^ Logical AND of several props (empty is true)
  | PropOr Prop Prop
  -- ^ Logical OR of several props (empty is false)
  | PropNext Prop
  -- ^ A prop that holds the next timestamp
  | PropUntil Prop Prop
  -- ^ 'PropUntil r1 r2' means 'eventually r2' and at least until 'r2' holds, 'r1' always holds.
  -- If both are false, the prop is false. When 'r2' holds, the prop is true.
  | PropRelease Prop Prop
  -- ^ 'PropRelease r1 r2' means 'always r2' until and including when 'r1' holds.
  -- If 'r2' is false, the prop is false. When 'r1' and 'r2' hold, the prop is true.
  | PropForAll !Binder Prop
  -- ^ 'PropForAll (Binder n t) r' means for all 'n' of type 't' 'r' holds.
  | PropExists !Binder Prop
  -- ^ 'PropForAll (Binder n t) r' means there exists an 'n' of type 't' for which 'r' holds.
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

-- Makes PropF and instances for Recursive and Corecursive from Data.Functor.Foldable
makeBaseFunctor ''Prop

deriving stock instance Generic (PropF a)
deriving anyclass instance Hashable a => (Hashable (PropF a))
deriving anyclass instance NFData a => (NFData (PropF a))

-- | Put the prop in negation normal form, which basically involves
-- pushing negations to the bottom.
--
-- >>> propNegationNormalForm (PropAtom (Atom "a" []))
-- PropAtom (Atom "a" [])
-- >>> propNegationNormalForm (PropNot (PropAtom (Atom "a" [])))
-- PropNot (PropAtom (Atom "a" []))
-- >>> propNegationNormalForm (PropNot (PropAnd (PropNot (PropAtom (Atom "a" []))) (PropAtom (Atom "b" []))))
-- PropOr (PropAtom (Atom "a" [])) (PropNot (PropAtom (Atom "b" [])))
propNegationNormalForm :: Prop -> Prop
propNegationNormalForm = pos where
  pos f =
    case project f of
      PropNotF r -> neg r
      fr -> embed (fmap pos fr)
  neg = \case
    PropAtom a -> PropNot (PropAtom a)
    PropTrue -> PropFalse
    PropFalse -> PropTrue
    PropNot r -> pos r
    PropAnd r1 r2 -> PropOr (neg r1) (neg r2)
    PropOr r1 r2 -> PropAnd (neg r1) (neg r2)
    PropNext r -> PropNext (neg r)
    PropUntil r1 r2 -> PropRelease (neg r1) (neg r2)
    PropRelease r1 r2 -> PropUntil (neg r1) (neg r2)
    PropForAll b r -> PropExists b (neg r)
    PropExists b r -> PropForAll b (neg r)

propAtom :: PropName -> [VarName] -> Prop
propAtom p args = PropAtom (Atom p args)

-- | AND all the given props together (empty is true).
propAndAll :: [Prop] -> Prop
propAndAll = \case
  [] -> PropTrue
  [r] -> r
  [r1, r2] -> PropAnd r1 r2
  r1:rs -> PropAnd r1 (propAndAll rs)

-- | OR all the given props together (empty is false).
propOrAll :: [Prop] -> Prop
propOrAll = \case
  [] -> PropFalse
  [r] -> r
  [r1, r2] -> PropOr r1 r2
  r1:rs -> PropOr r1 (propOrAll rs)

-- | A prop that holds at every timestep. If it is ever false, the prop is false.
propAlways :: Prop -> Prop
propAlways = PropRelease PropFalse

-- | A prop that will hold at some timestep. If it is ever true, the prop is true.
propEventually :: Prop -> Prop
propEventually = PropUntil PropTrue

-- | Propositional implication: r1 -> r2
propIf :: Prop -> Prop -> Prop
propIf = PropOr . PropNot

-- | Simple constructor for nested ifs
propIfNested :: [Prop] -> Prop -> Prop
propIfNested hyps body = go hyps where
  go = \case
    [] -> body
    hyp:hyps' -> propIf hyp (go hyps')

-- | Bidiriectional propositional implication: r1 <-> r2
propIff :: Prop -> Prop -> Prop
propIff r1 r2 = PropAnd (propIf r1 r2) (propIf r2 r1)

-- | Simple constructor for nested foralls.
propForAllNested :: [(VarName, TyName)] -> Prop -> Prop
propForAllNested pairs body = go pairs where
  go = \case
    [] -> body
    (v, t):rest -> PropForAll (Binder v t) (go rest)

-- | Simple constructor for nested exists.
propExistsNested :: [(VarName, TyName)] -> Prop -> Prop
propExistsNested pairs body = go pairs where
  go = \case
    [] -> body
    (v, t):rest -> PropExists (Binder v t) (go rest)

-- | The size of the 'Prop' (number of constructors)
--
-- >>> propSize (PropAtom (Atom "a" []))
-- 1
-- >>> propSize (PropUntil (PropAtom (Atom "a" [])) (PropNot (PropAtom (Atom "b" []))))
-- 4
--
propSize :: Prop -> Int
propSize = getSum . fold ((Sum 1 <>) . sum)

-- | The depth of the 'Prop' (max length from root to leaf)
--
-- >>> propDepth (PropAtom (Atom "a" []))
-- 1
-- >>> propDepth (PropUntil (PropAtom (Atom "a" [])) (PropNot (PropAtom (Atom "b" []))))
-- 2
--
propDepth :: Prop -> Int
propDepth = getMax . fold ((Max 1 <>) . sum)

-- | Gathers all the unique atoms in the proposition
--
-- >>> propAtoms (PropUntil (PropAtom (Atom "a" [])) (PropAtom (Atom "b" [])))
-- [Atom "a" [],Atom "b" []]
--
propAtoms :: Prop -> [Atom]
propAtoms = execWriter . foldUpM go where
  go = \case
    PropAtomF a -> tell [a]
    _ -> pure ()

-- -- | When we evaluate a proposition at a certain time step, we either
-- -- satisfy it, falsify it, or are left with another prop to evaluate
-- -- on the next timestep.
-- data PropRes x =
--     PropResTrue
--   | PropResFalse
--   | PropResNext !x
--   deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)
--   deriving anyclass (Hashable, NFData)

-- -- | Negates the result
-- propResNot :: PropRes (Prop p) -> PropRes (Prop p)
-- propResNot = \case
--   PropResTrue -> PropResFalse
--   PropResFalse -> PropResTrue
--   PropResNext r -> PropResNext (Prop (PropNotF r))

-- -- | Evaluate the proposition at the current timestep with the given evaluation function.
-- -- (See also graphEval.)
-- propEval :: (p -> Bool) -> Prop p -> PropRes (Prop p)
-- propEval f = go where
--   go p0@(Prop f0) =
--     case f0 of
--       PropAtomF p -> if f p then PropResTrue else PropResFalse
--       PropTrueF -> PropResTrue
--       PropFalseF -> PropResFalse
--       PropNotF r -> propResNot (go r)
--       PropAndF r1 r2 ->
--         case go r1 of
--           PropResTrue -> go r2
--           PropResFalse -> PropResFalse
--           n@(PropResNext r1') ->
--             case go r2 of
--               PropResTrue -> n
--               PropResFalse -> PropResFalse
--               PropResNext r2' -> PropResNext (Prop (PropAndF r1' r2'))
--       PropOrF r1 r2 ->
--         case go r1 of
--           PropResTrue -> PropResTrue
--           PropResFalse -> go r1
--           n@(PropResNext r1') ->
--             case go r2 of
--               PropResTrue -> PropResTrue
--               PropResFalse -> n
--               PropResNext r2' -> PropResNext (Prop (PropOrF r1' r2'))
--       PropNextF r -> PropResNext r
--       -- See "Until" logic from "Runtime Verification of Concurrent Haskell Programs" s3.2, p7.
--       PropUntilF r1 r2 ->
--         case go r2 of
--           -- Once r2 holds, the proposition is satisfied
--           PropResTrue -> PropResTrue
--           -- If r2 does not hold,
--           PropResFalse ->
--             case go r1 of
--               -- If r1 does not hold, the proposition is falsified
--               PropResFalse -> PropResFalse
--               -- If r1 holds, we are still in the until
--               PropResTrue -> PropResNext p0
--               -- If r1 advances, we need to satisfy the new prop and the existing until prop
--               PropResNext r1' -> PropResNext (Prop (PropAndF r1' p0))
--           -- If r2 advances,
--           PropResNext r2' ->
--             case go r1 of
--               -- If r1 does not hold, then we are absolved of the until only if r2' holds
--               PropResFalse -> PropResNext r2'
--               -- If r1 does hold, then we can either wait for absolution or keep on with the until
--               PropResTrue -> PropResNext (Prop (PropOrF r2' p0))
--               -- If r1 advances, we follow similar logic
--               PropResNext r1' -> PropResNext (Prop (PropOrF r2' (Prop (PropAndF r1' p0))))
--       PropReleaseF r1 r2 ->
--         case go r2 of
--           -- If r2 does not hold, the proposition is falsified
--           PropResFalse -> PropResFalse
--           -- If r2 holds,
--           PropResTrue ->
--             case go r1 of
--               -- If r1 does not hold, we are still in the release
--               PropResFalse -> PropResNext p0
--               -- If r1 holds, the proposition is satisfied
--               PropResTrue -> PropResTrue
--               -- If r1 advances, we need to satisfy the new prop and the existing release prop
--               PropResNext _ -> error "TODO" -- PropResNext (Prop (PropAndF [r1', p0]))
--           -- If r2 advances,
--           PropResNext _ ->
--             case go r1 of
--               PropResFalse -> error "TODO"
--               PropResTrue -> error "TODO"
--               PropResNext _ -> error "TODO"

-- -- | Evaluate the prop at every timestep until true/false or there are no more inputs.
-- -- Also returns the number of timesteps evaluated.
-- propFold :: (a -> p -> Bool) -> Prop p -> [a] -> (Int, PropRes (Prop p))
-- propFold f = go 0 where
--   go i p xs =
--     case xs of
--       [] -> (i, PropResNext p)
--       y:ys ->
--         let i' = i + 1
--             r = propEval (f y) p
--         in case r of
--           PropResNext p' -> go i' p' ys
--           _ -> (i', r)

-- | A (state, action, state) triple - used for defining worlds.
data SAS s a = SAS
  { sasBefore :: !s
  , sasAction :: !a
  , sasAfter :: !s
  } deriving stock (Eq, Show)

-- | Scan a list of actions into a list of SAS
scanSAS :: (a -> s -> s) -> s -> [a] -> [SAS s a]
scanSAS update initState actions = result where
  result = case actions  of
    [] -> []
    a:as -> scanr go (initWorld a) as
  initWorld a = SAS initState a (update a initState)
  go a (SAS _ _ after) = SAS after a (update a after)
