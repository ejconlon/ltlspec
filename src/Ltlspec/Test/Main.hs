module Ltlspec.Test.Main (main) where

import qualified Data.Map.Strict as Map
import Ltlspec (Atom (..), Binder (..), Bridge (..), Prop (..), Theory (..), VarName, propForAllNested, propIf,
                propIfNested)

-- import Data.Foldable (toList)
-- import Data.Hashable (Hashable)
-- import GHC.Generics (Generic)
-- import Hedgehog (Gen, Property, PropertyT, forAll, property)
-- import Ltlspec (Prop, PropRes (..), pattern PropAtom, propAlways, propAtoms, propEval, propEventually, propFold)
-- import Test.Tasty (TestName, TestTree, defaultMain, testGroup)
-- import Test.Tasty.HUnit (testCase, (@?=))
-- import Test.Tasty.Hedgehog (testProperty)

-- class Unconstrained a
-- instance Unconstrained a

-- newtype ConstrainedTrans c g m = ConstrainedTrans { unConstrainedTrans :: forall x. c x => g x -> m x }

-- idTrans :: ConstrainedTrans Unconstrained m m
-- idTrans = ConstrainedTrans id

-- propertyTrans :: ConstrainedTrans Show Gen (PropertyT IO)
-- propertyTrans = ConstrainedTrans forAll

-- data CheckPhase s a =
--     CheckPhaseStart
--   | CheckPhaseAct !Int ![a] !s !a
--   | CheckPhaseEnd !Int ![a]
--   deriving (Eq, Show)

-- data Kase g m s a = Kase
--   { kaseMkStart :: g s
--   , kaseMkNext :: s -> g (Maybe a)
--   , kaseAct :: s -> a -> m s
--   , kaseCheck :: CheckPhase s a -> s -> m ()
--   }

-- runKaseGeneric :: (Monad m, c s, c (Maybe a)) => ConstrainedTrans c g m -> Kase g m s a -> m ()
-- runKaseGeneric trans (Kase mkStart mkNext act check) = go where
--   go = do
--     start <- unConstrainedTrans trans mkStart
--     check CheckPhaseStart start
--     loop 0 [] start
--   loop !i !hist !state = do
--     mayNext <- unConstrainedTrans trans (mkNext state)
--     case mayNext of
--       Nothing -> check (CheckPhaseEnd i hist) state
--       Just next -> do
--         state' <- act state next
--         check (CheckPhaseAct i hist state next) state'
--         loop (i + 1) (next:hist) state'

-- runKase :: Monad m => Kase m m s a -> m ()
-- runKase = runKaseGeneric idTrans

-- runKaseProperty :: (Show s, Show a) => Kase Gen (PropertyT IO) s a -> PropertyT IO ()
-- runKaseProperty = runKaseGeneric propertyTrans

-- kaseProperty :: (Show s, Show a) => Kase Gen (PropertyT IO) s a -> Property
-- kaseProperty = property . runKaseProperty

-- testKaseProperty :: (Show s, Show a) => TestName -> Kase Gen (PropertyT IO) s a -> TestTree
-- testKaseProperty name = testProperty name . kaseProperty

-- testKaseUnit :: TestName -> Kase IO IO s a -> TestTree
-- testKaseUnit name = testCase name . runKase

-- data Comp = CompLT | CompLTE | CompEQ | CompNEQ | CompGTE | CompGT
--   deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
--   deriving anyclass (Hashable)

-- compNot :: Comp -> Comp
-- compNot = \case
--   CompLT -> CompGTE
--   CompLTE -> CompGT
--   CompEQ -> CompNEQ
--   CompNEQ -> CompEQ
--   CompGTE -> CompLT
--   CompGT -> CompLTE

-- data OrdPred a = OrdPred
--   { ordPredComp :: !Comp
--   , ordPredRef :: !a
--   } deriving stock (Eq, Ord, Show, Generic)
--     deriving anyclass (Hashable)

-- ordPredNot :: OrdPred a -> OrdPred a
-- ordPredNot (OrdPred c a) = OrdPred (compNot c) a

-- ordPredEval :: Ord a => a -> OrdPred a -> Bool
-- ordPredEval val (OrdPred comp ref) =
--   case comp of
--     CompLT -> val < ref
--     CompLTE -> val <= ref
--     CompEQ -> val == ref
--     CompNEQ -> val /= ref
--     CompGTE -> val >= ref
--     CompGT -> val > ref

-- type OrdPredProp a = Prop (OrdPred a)
-- type OrdPredPropRes a = PropRes (OrdPredProp a)

-- ordPredPropEval :: Ord a => a -> OrdPredProp a -> OrdPredPropRes a
-- ordPredPropEval val = propEval (ordPredEval val)

-- ordPredPropFold :: Ord a => OrdPredProp a -> [a] -> (Int, OrdPredPropRes a)
-- ordPredPropFold = propFold ordPredEval

eqForAll :: [VarName] -> Prop -> Prop
eqForAll = propForAllNested . fmap (, "Value")

eqProp :: VarName -> VarName -> Prop
eqProp x y = PropAtom (Atom "IsEq" [x, y])

eqAxReflexive, eqAxTransitive, eqAxSymmetric :: Prop
eqAxReflexive = eqForAll ["x"] (eqProp "x" "x")
eqAxTransitive = eqForAll ["x", "y", "z"] (propIfNested [eqProp "x" "y", eqProp "y" "z"] (eqProp "x" "z"))
eqAxSymmetric = eqForAll ["x", "y"] (propIf (eqProp "x" "y") (eqProp "y" "x"))

-- A theory of total order over some type
orderTheory :: Theory
orderTheory = Theory
  { theoryTypes = ["Value"]
  , theoryProps = Map.fromList
      [ ("IsEq", ["Value", "Value"])
      ]
  , theoryAxioms = Map.fromList
      [ ("Reflexive", eqAxReflexive)
      , ("Transitive", eqAxTransitive)
      , ("Symmetric", eqAxSymmetric)
      ]
  }

newtype EqErr = EqErr { unEqErr :: String }
  deriving (Eq, Show)

data EqValue v =
    EqValueNormal v
  | EqValueNever
  | EqValueErr !EqErr
  deriving (Eq, Show)

eqEval :: Eq v => EqValue v -> EqValue v -> Either EqErr Prop
eqEval a b =
  case (a, b) of
    (EqValueNever, _) -> Right PropFalse
    (_, EqValueNever) -> Right PropFalse
    (EqValueErr err, _) -> Left err
    (_, EqValueErr err) -> Left err
    (EqValueNormal x, EqValueNormal y) -> Right (if x == y then PropTrue else PropFalse)

newtype EqWorld v = EqWorld { unEqWorld :: [EqValue v] }
  deriving stock (Eq, Show)

instance Eq v => Bridge EqErr (EqValue v) (EqWorld v) where
  bridgeEvalProp _ (Atom propName vals) =
    case (propName, vals) of
      ("IsEq", [v1, v2]) -> eqEval v1 v2
      _ -> Left (EqErr "Bad prop")
  bridgeQuantify (EqWorld eqvs) tyName =
    case tyName of
      "Value" -> Right eqvs
      _ -> Left (EqErr "Bad type")

-- testEventually :: TestTree
-- testEventually = testCase "eventually" $ do
--   let prop = propEventually (PropAtom (OrdPred CompEQ 'e'))
--   toList (propAtoms prop) @?= [OrdPred CompEQ 'e']
--   ordPredPropEval 'a' prop @?= PropResNext prop
--   ordPredPropFold prop "abcdefg" @?= (5, PropResTrue)

-- testAlways :: TestTree
-- testAlways = testCase "always" $ do
--   let prop = propAlways (PropAtom (OrdPred CompLT 'z'))
--   toList (propAtoms prop) @?= [OrdPred CompLT 'z']
--   ordPredPropEval 'a' prop @?= PropResNext prop
--   ordPredPropFold prop "abcdefg" @?= (7, PropResNext prop)

-- testProp :: TestTree
-- testProp = testGroup "Prop" [testEventually, testAlways]

main :: IO ()
main = pure () -- defaultMain (testGroup "Ltlspec" [testProp])
