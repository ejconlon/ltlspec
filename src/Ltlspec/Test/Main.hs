module Ltlspec.Test.Main (main) where

import qualified Data.Map.Strict as Map
import Ltlspec (envPropFold, propForAllNested, propIf, propIfNested)
import Ltlspec.Types (Atom (..), Bridge (..), EnvProp (..), EnvPropGood (..), EnvPropRes, EnvPropStep (..), Prop (..),
                      Theory (..), VarName)
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

eqCases :: [EqCase Char]
eqCases =
  [ EqCase "null false" [] PropFalse 0 (Right (EnvPropGoodNext (EnvPropStepSingle (EnvProp mempty PropFalse))))
  , EqCase "null true" [] PropTrue 0 (Right (EnvPropGoodNext (EnvPropStepSingle (EnvProp mempty PropTrue))))
  , EqCase "empty false" [EqWorld []] PropFalse 1 (Right (EnvPropGoodBool False))
  , EqCase "empty true" [EqWorld []] PropTrue 1 (Right (EnvPropGoodBool True))
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
