{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | Basic types - this is here to keep TemplateHaskell to one file.
module Ltlspec.Types where

import Control.DeepSeq (NFData)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import GHC.Generics (Generic)

type Error = String

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
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

type VarName = String

data Atom v = Atom !PropName ![v]
  deriving stock (Eq, Show, Generic, Functor, Foldable, Traversable)
  deriving anyclass (Hashable, NFData)

type AtomVar = Atom VarName

data Binder = Binder !VarName !TyName
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

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

-- | Variables bound during prop eval
type Env v = Seq (VarName, v)

-- | Just a named tuple of environment and prop
data EnvProp v = EnvProp
  { epEnv :: !(Env v)
  , epProp :: !Prop
  } deriving stock (Eq, Show, Generic)
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
  | EnvPropStepQuant !Quantifier !(Seq (EnvPropStep v))
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

-- | A (state, action, state) triple - used for defining worlds.
data SAS s a = SAS
  { sasBefore :: !s
  , sasAction :: !a
  , sasAfter :: !s
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | A 'Bridge' is something that can eval props and quantify in a given world.
-- `w` is world type, `e` is error type, `v` is value type.
-- Typeclass-wise we associate instances with the world type. `w -> e v` means
-- the world type determines the others.
-- This is an "interpretation" in the logic sense.
class Bridge e v w | w -> e v where
  -- | Evaluate the atomic proposition or fail.
  bridgeEvalProp :: w -> Atom v -> Either e Prop
  -- | Quantify over all values of the given type or fail.
  bridgeQuantify :: w -> TyName -> Either e [v]
