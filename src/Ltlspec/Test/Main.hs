module Ltlspec.Test.Main (main) where

import qualified Data.Map.Strict as Map
import Ltlspec (envPropFold, propAlways, propEventually, propForAllNested, propIf, propIfNested)
import Ltlspec.Types (Atom (..), Binder (..), Bridge (..), EnvProp (..), EnvPropBad (..), EnvPropGood (..), EnvPropRes,
                      EnvPropStep (..), Prop (..), Theory (..), VarName)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
-- import Data.Sequence (Seq)
-- import qualified Data.Sequence as Seq
import Control.Monad (when)
import System.Environment (lookupEnv, setEnv)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stderr, stdout)

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

data EqCase v = EqCase
  { eqCaseName :: !String
  , eqCaseWorlds :: ![EqWorld v]
  , eqCaseProp :: !Prop
  , eqCaseSteps :: !Int
  , eqCaseRes :: !(EnvPropRes EqErr (EqValue v))
  } deriving stock (Eq, Show)

testEqCase :: (Eq v, Show v) => EqCase v -> TestTree
testEqCase (EqCase name worlds prop expectedSteps expectedRes) = testCase name $ do
  let actualPair = envPropFold (EnvProp mempty prop) worlds
  actualPair @?= (expectedSteps, expectedRes)

defaultErr :: EqErr
defaultErr = EqErr "You are doomed!"

emptyWorld :: EqWorld v
emptyWorld = EqWorld []

-- A good fake world
fakeWorld1 :: EqWorld Int
fakeWorld1 = EqWorld [EqValueNormal 1, EqValueNormal 2, EqValueNormal 3]

-- A bad fake world, EqValueNever does not equal to itself!
fakeWorld2 :: EqWorld Int
fakeWorld2 = EqWorld [EqValueNormal 1, EqValueNormal 2, EqValueNormal 3, EqValueNever]

-- You cannot be more badass than this ;-)
fakeWorld3 :: EqWorld Int
fakeWorld3 = EqWorld [EqValueNever, EqValueNever, EqValueNever]

-- Turns out you can...
fakeWorld4 :: EqWorld Int
fakeWorld4 = EqWorld [EqValueErr defaultErr, EqValueErr defaultErr]

longWorlds1 :: [EqWorld Int]
longWorlds1 = [fakeWorld3, fakeWorld3, fakeWorld3, fakeWorld3, fakeWorld3, fakeWorld3, fakeWorld3, fakeWorld1]

longWorlds2 :: [EqWorld Int]
longWorlds2 = [fakeWorld1, fakeWorld1, fakeWorld1, fakeWorld1, fakeWorld3, fakeWorld1, fakeWorld1, fakeWorld1]

eqCases :: [EqCase Int]
eqCases =
  [ EqCase "0 world; no eval1 #?" [] PropFalse 0 (Right (EnvPropGoodNext (EnvPropStepSingle (EnvProp mempty PropFalse))))
  , EqCase "0 world; no eval2 #?" [] PropTrue 0 (Right (EnvPropGoodNext (EnvPropStepSingle (EnvProp mempty PropTrue))))
  , EqCase "1 world; true #t" [emptyWorld] PropTrue 1 (Right (EnvPropGoodBool True))
  , EqCase "1 world; false #f" [emptyWorld] PropFalse 1 (Right (EnvPropGoodBool False))
  , EqCase "1 world; and1 #t" [emptyWorld] (PropAnd PropTrue PropTrue) 1 (Right (EnvPropGoodBool True))
  , EqCase "1 world; and2 #f" [emptyWorld] (PropAnd PropTrue PropFalse) 1 (Right (EnvPropGoodBool False))
  , EqCase "1 world; or1 #t" [emptyWorld] (PropOr PropTrue PropFalse) 1 (Right (EnvPropGoodBool True))
  , EqCase "1 world; or2 #f" [emptyWorld] (PropOr PropFalse PropFalse) 1 (Right (EnvPropGoodBool False))
  , EqCase "1 world; next1 #t" [emptyWorld] (PropNext (PropTrue)) 1 (Right (EnvPropGoodNext (EnvPropStepSingle (EnvProp mempty PropTrue))))
  , EqCase "3 world; next2 #t" [emptyWorld, emptyWorld, emptyWorld] (PropNext (PropTrue)) 2 (Right (EnvPropGoodBool True))
  , EqCase "3 world; next3 #?" [emptyWorld, emptyWorld, emptyWorld] (PropNext (PropNext (PropTrue))) 3 (Right (EnvPropGoodBool True))
  , EqCase "1 world; vacuous forall #t" [emptyWorld] (PropForAll (Binder "a" "Value") (PropAtom (Atom "IsEq" ["b", "b"]))) 1 (Right (EnvPropGoodBool True))
  , EqCase "1 world; forall1 #t" [fakeWorld1] (PropForAll (Binder "a" "Value") (PropAtom (Atom "IsEq" ["a", "a"]))) 1 (Right (EnvPropGoodBool True))
  , EqCase "1 world; forall2 #f" [fakeWorld2] (PropForAll (Binder "a" "Value") (PropAtom (Atom "IsEq" ["a", "a"]))) 1 (Right (EnvPropGoodBool False))
  , EqCase "1 world; forall3 #e" [fakeWorld4] (PropForAll (Binder "a" "Value") (PropAtom (Atom "IsEq" ["a", "a"]))) 1 (Left (EnvPropBadErr defaultErr))
  , EqCase "1 world; forall4 #e" [fakeWorld1] (PropForAll (Binder "a" "Value") (PropAtom (Atom "IsEq" ["b", "b"]))) 1 (Left (EnvPropBadMissing "b"))
  , EqCase "1 world; vacuous exists #f" [emptyWorld] (PropExists (Binder "a" "Value") (PropAtom (Atom "IsEq" ["a", "a"]))) 1 (Right (EnvPropGoodBool False))
  , EqCase "1 world; exists1 #t" [fakeWorld2] (PropExists (Binder "a" "Value") (PropAtom (Atom "IsEq" ["a", "a"]))) 1 (Right (EnvPropGoodBool True))
  , EqCase "1 world; exists2 #f" [fakeWorld3] (PropExists (Binder "a" "Value") (PropAtom (Atom "IsEq" ["a", "a"]))) 1 (Right (EnvPropGoodBool False))
  , EqCase "2 world; next forall #t" [emptyWorld, fakeWorld1] (PropNext (PropForAll (Binder "a" "Value") (PropAtom (Atom "IsEq" ["a", "a"])))) 2 (Right (EnvPropGoodBool True))
  , EqCase "2 world; next exists #t" [emptyWorld, fakeWorld2] (PropNext (PropExists (Binder "a" "Value") (PropAtom (Atom "IsEq" ["a", "a"])))) 2 (Right (EnvPropGoodBool True))
  , EqCase "1 world; nested forall #f" [fakeWorld1] (PropForAll (Binder "a" "Value") (PropForAll (Binder "b" "Value") (PropAtom (Atom "IsEq" ["a", "b"])))) 1 (Right (EnvPropGoodBool False))
  , EqCase "1 world; forall exists #t" [fakeWorld1] (PropForAll (Binder "a" "Value") (PropExists (Binder "b" "Value") (PropAtom (Atom "IsEq" ["a", "b"])))) 1 (Right (EnvPropGoodBool True))
  , EqCase "1 world; and forall #t" [fakeWorld1] (PropAnd (PropForAll (Binder "a" "Value") (PropAtom (Atom "IsEq" ["a", "a"]))) (PropForAll (Binder "a" "Value") (PropAtom (Atom "IsEq" ["a", "a"])))) 1 (Right (EnvPropGoodBool True))
  , EqCase "8 world; eventually #t" longWorlds1 (propEventually (PropForAll (Binder "a" "Value") (PropAtom (Atom "IsEq" ["a", "a"])))) 8 (Right (EnvPropGoodBool True))
  , EqCase "8 world; always #f" longWorlds2 (propAlways (PropForAll (Binder "a" "Value") (PropAtom (Atom "IsEq" ["a", "a"])))) 5 (Right (EnvPropGoodBool False))
  , EqCase "2 world; and next forall1 #t" [fakeWorld3, fakeWorld1] (PropAnd (PropNext (PropForAll (Binder "a" "Value") (PropAtom (Atom "IsEq" ["a", "a"])))) (PropNext (PropForAll (Binder "a" "Value") (PropAtom (Atom "IsEq" ["a", "a"]))))) 2 (Right (EnvPropGoodBool True))
  , EqCase "2 world; and next forall2 #f" [fakeWorld3, fakeWorld2] (PropAnd (PropNext (PropForAll (Binder "a" "Value") (PropAtom (Atom "IsEq" ["a", "a"])))) (PropNext (PropForAll (Binder "a" "Value") (PropAtom (Atom "IsEq" ["a", "a"]))))) 2 (Right (EnvPropGoodBool False))
  , EqCase "2 world; or next forall exist #t" [fakeWorld3, fakeWorld2] (PropOr (PropNext (PropForAll (Binder "a" "Value") (PropAtom (Atom "IsEq" ["a", "a"])))) (PropNext (PropExists (Binder "a" "Value") (PropAtom (Atom "IsEq" ["a", "a"]))))) 2 (Right (EnvPropGoodBool True))
  ]

testEqCases :: TestTree
testEqCases = testGroup "Eq cases" (fmap testEqCase eqCases)

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
main = do
  mayDebugStr <- lookupEnv "DEBUG"
  let debug = Just "1" == mayDebugStr
  when debug $ do
    setEnv "TASTY_NUM_THREADS" "1"
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering
  defaultMain $ testGroup "Ltlspec"
    [ testEqCases
    ]
