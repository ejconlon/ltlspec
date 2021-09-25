module Main (main) where

import Data.Foldable (toList)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Hedgehog (Gen, Property, PropertyT, forAll, property)
import Ltlspec (Prop, PropRes (..), pattern PropAtom, pattern PropEventually, propAtoms, propEval, propFold)
import Test.Tasty (TestName, TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

class Unconstrained a
instance Unconstrained a

newtype ConstrainedTrans c g m = ConstrainedTrans { unConstrainedTrans :: forall x. c x => g x -> m x }

idTrans :: ConstrainedTrans Unconstrained m m
idTrans = ConstrainedTrans id

propertyTrans :: ConstrainedTrans Show Gen (PropertyT IO)
propertyTrans = ConstrainedTrans forAll

data CheckPhase =
    CheckPhaseStart
  | CheckPhaseNext !Int
  | CheckPhaseAct !Int
  deriving stock (Eq, Ord, Show)

data Kase g m s a = Kase
  { kaseMkStart :: g s
  , kaseMkNext :: Int -> s -> g (Maybe (a, s))
  , kaseCheck :: CheckPhase -> s -> m ()
  , kaseAct :: Int -> s -> a -> m s
  }

runKaseGeneric :: (Monad m, c s, c (Maybe (a, s))) => ConstrainedTrans c g m -> Kase g m s a -> m ()
runKaseGeneric trans (Kase mkStart mkNext check act) = go where
  go = do
    start <- unConstrainedTrans trans mkStart
    check CheckPhaseStart start
    loop 0 start
  loop !i !state = do
    step <- unConstrainedTrans trans (mkNext i state)
    case step of
      Nothing -> pure ()
      Just (next, state') -> do
        check (CheckPhaseNext i) state'
        state'' <- act i state' next
        check (CheckPhaseAct i) state''
        loop (i + 1) state''

runKase :: Monad m => Kase m m s a -> m ()
runKase = runKaseGeneric idTrans

runKaseProperty :: (Show s, Show a) => Kase Gen (PropertyT IO) s a -> PropertyT IO ()
runKaseProperty = runKaseGeneric propertyTrans

kaseProperty :: (Show s, Show a) => Kase Gen (PropertyT IO) s a -> Property
kaseProperty = property . runKaseProperty

testKaseProperty :: (Show s, Show a) => TestName -> Kase Gen (PropertyT IO) s a -> TestTree
testKaseProperty name = testProperty name . kaseProperty

testKaseUnit :: TestName -> Kase IO IO s a -> TestTree
testKaseUnit name = testCase name . runKase

data Comp = CompLT | CompLTE | CompEQ | CompNEQ | CompGTE | CompGT
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable)

compNot :: Comp -> Comp
compNot = \case
  CompLT -> CompGTE
  CompLTE -> CompGT
  CompEQ -> CompNEQ
  CompNEQ -> CompEQ
  CompGTE -> CompLT
  CompGT -> CompLTE

data OrdPred a = OrdPred
  { ordPredComp :: !Comp
  , ordPredRef :: !a
  } deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Hashable)

ordPredNot :: OrdPred a -> OrdPred a
ordPredNot (OrdPred c a) = OrdPred (compNot c) a

ordPredEval :: Ord a => a -> OrdPred a -> Bool
ordPredEval val (OrdPred comp ref) =
  case comp of
    CompLT -> val < ref
    CompLTE -> val <= ref
    CompEQ -> val == ref
    CompNEQ -> val /= ref
    CompGTE -> val >= ref
    CompGT -> val > ref

type OrdPredProp a = Prop (OrdPred a)
type OrdPredPropRes a = PropRes (OrdPred a)

ordPredPropEval :: Ord a => a -> OrdPredProp a -> OrdPredPropRes a
ordPredPropEval val = propEval (ordPredEval val)

ordPredPropFold :: Ord a => OrdPredProp a -> [a] -> (Int, OrdPredPropRes a)
ordPredPropFold = propFold ordPredEval

testSimple :: TestTree
testSimple = testCase "simple" $ do
    let prop = PropEventually (PropAtom (OrdPred CompEQ 'e'))
    toList (propAtoms prop) @?= [OrdPred CompEQ 'e']
    ordPredPropEval 'a' prop @?= PropResNext prop
    ordPredPropFold prop "abcdefg" @?= (5, PropResTrue)

main :: IO ()
main = defaultMain (testGroup "Ltlspec" [testSimple])
