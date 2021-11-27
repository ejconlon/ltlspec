{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | Basic types - this is here to keep TemplateHaskell to one file.
module Ltlspec.Types where

import Control.DeepSeq (NFData)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Hashable (Hashable)
import Data.List (scanl')
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.Set (Set)
import GHC.Generics (Generic)
import Ltlspec.TriBool (TriBool)

type Error = String

data Commented a =
    NoComment !a
  | YesComment !a !String
  deriving stock (Eq, Show, Generic, Functor, Foldable, Traversable)
  deriving anyclass (NFData)

withoutComment :: Commented a -> a
withoutComment = \case
  NoComment a -> a
  YesComment a _ -> a

type PropName = String
type TyName = String
type AxiomName = String

type VarName = String

data Atom v = Atom !PropName ![v]
  deriving stock (Eq, Show, Generic, Functor, Foldable, Traversable)
  deriving anyclass (Hashable, NFData)

type AtomVar = Atom VarName

data Binder = Binder !VarName !TyName
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

data BinderGroup = BinderGroup ![VarName] !TyName
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

-- | "Sugared" proposition.
-- Allows for pretty printing of the proposition but
-- is quickly desugared to 'Prop'.
data SProp =
    SPropAtom !AtomVar
  | SPropTrue
  | SPropFalse
  | SPropNot !SProp
  | SPropAnd ![SProp]
  | SPropOr ![SProp]
  | SPropIf ![SProp] !SProp
  | SPropIff !SProp !SProp
  | SPropNext SProp
  | SPropAlways !SProp
  | SPropEventually !SProp
  | SPropUntil !SProp !SProp
  | SPropRelease !SProp !SProp
  | SPropForAll ![BinderGroup] !SProp
  | SPropExists ![BinderGroup] !SProp
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

-- Makes SPropF and instances for Recursive and Corecursive from Data.Functor.Foldable
makeBaseFunctor ''SProp

deriving stock instance Generic (SPropF a)
deriving anyclass instance Hashable a => (Hashable (SPropF a))
deriving anyclass instance NFData a => (NFData (SPropF a))

-- | An LTL proposition with first-order data quantification.
-- This selection of operators corresponds to "Release Positive Normal Form"
data Prop =
    PropAtom !AtomVar
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

type TyDef = Commented TyName
type TyDefs = [TyDef]
type PropDef = (PropName, Commented [TyName])
type PropDefs = Map PropName (Commented [TyName])
type AxiomDef = (AxiomName, Commented SProp)
type AxiomDefs = Map AxiomName (Commented SProp)

data Theory = Theory
  { theoryTypes :: !TyDefs
  , theoryProps :: !PropDefs
  , theoryAxioms :: !AxiomDefs
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Variables bound during prop eval
type Env v = Map VarName v

-- | Named tuple of environment and prop
-- NOTE: only propAtom needs environment
-- All other composed props just need to carry child props with correct environments
data EnvProp v =
    EnvProp !(Env v) !Prop
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Quantifiers: we have forall and exists
data Quantifier =
    QuantifierForAll
  | QuantifierExists
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | A step a proposition can take from one timestep to the next
data EnvPropStep v =
    EnvPropStepSingle !(EnvProp v)
  -- ^ A non-quantified step
  | EnvPropStepParallel !Quantifier !(Seq (EnvPropStep v))
  -- ^ A quantified step
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Good outcomes for prop evaluation
data EnvPropGood v =
    EnvPropGoodBool !Bool
  -- ^ The prop was fully evaluated to true/false in the given environment
  | EnvPropGoodNext !(EnvPropStep v)
  -- ^ The prop requires further evaluation in next timestep
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Bad outcomes for prop evaluation
data EnvPropBad e =
    EnvPropBadErr !e
  -- ^ An error raised by the bridge
  | EnvPropBadMissing !VarName
  -- ^ Unbound variable error
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | The result of evaluating an 'EnvProp'
type EnvPropRes e v = Either (EnvPropBad e) (EnvPropGood v)

class ApplyAction a w | w -> a where
  -- | Apply the action to the given world and yield a new world
  applyAction :: w -> a -> w
  -- | Apply a sequence of actions and yield the corresponding sequence of worlds
  scanActions :: w -> [a] -> [w]
  scanActions = scanl' applyAction

-- | A (state, action, state) triple - used for defining worlds.
data SAS s a = SAS
  { sasBefore :: !s
  , sasAction :: !a
  , sasAfter :: !s
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Lift an apply action on states to an apply action on SAS triples.
applySAS :: (s -> a -> s) -> (SAS s a -> a -> SAS s a)
applySAS update (SAS _ _ after) a = SAS after a (update after a)

-- | Creates an initial SAS from an initial state and world
initSAS :: (s -> a -> s) -> s -> a -> SAS s a
initSAS update initState a = SAS initState a (update initState a)

-- | Scan a list of actions into a list of SAS
initScanSAS :: (s -> a -> s) -> s -> [a] -> [SAS s a]
initScanSAS update initState actions = result where
  result = case actions of
    [] -> []
    a:as -> scanl' (applySAS update) (initSAS update initState a) as

instance ApplyAction a s => ApplyAction a (SAS s a) where
  applyAction = applySAS applyAction

-- | A 'Bridge' is something that can eval props and quantify in a given world.
-- `w` is world type, `e` is error type, `v` is value type.
-- Typeclass-wise we associate instances with the world type. `w -> e v` means
-- the world type determines the others.
-- This is an "interpretation" in the logic sense.
class Eq v => Bridge e v w | w -> e v where
  -- | Evaluate the atomic proposition or fail.
  bridgeEvalProp :: w -> Atom v -> Either e Prop
  -- | Quantify over all values of the given type or fail.
  bridgeQuantify :: w -> TyName -> Either e [v]

-- | A 'Bridge' that supports proposition truncation.
class Bridge e v w => TruncBridge e v w where
  -- | Returns set of types with empty quantification in *all reachable worlds*
  truncBridgeEmpty :: w -> Set TyName
  -- | An oracle for atomic propositions in *all reachable worlds*
  truncBridgeOracle :: w -> Atom v -> Either e TriBool
