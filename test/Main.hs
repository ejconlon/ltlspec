module Main (main) where

import Data.Foldable (toList)
import Data.Hashable (Hashable)
import GHC.Generics
import Ltlspec
    ( evalProp,
      PropRes(PropResTrue, PropResNext),
      Prop(Prop),
      foldProp,
      PropF(PropAtom, PropEventually) )
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

newtype EqPred a = EqPred a
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

mkEqPred :: Eq a => a -> EqPred a -> Bool
mkEqPred val (EqPred x) = x == val

type PredProp a = Prop (EqPred a)
type PredPropRes a = PropRes (EqPred a)

mkEventuallyPredProp :: a -> PredProp a
mkEventuallyPredProp a = Prop (PropEventually (Prop (PropAtom (EqPred a))))

evalPredProp :: Eq a => a -> PredProp a -> PredPropRes a
evalPredProp val = evalProp (mkEqPred val)

foldPredProp :: Eq a => PredProp a -> [a] -> (Int, PredPropRes a)
foldPredProp = foldProp mkEqPred

testSimple :: TestTree
testSimple = testCase "simple" $ do
    let prop = mkEventuallyPredProp 'e'
    toList (propAtoms prop) @?= [EqPred 'e']
    evalPredProp 'a' prop @?= PropResNext prop
    foldPredProp prop "abcdefg" @?= (5, PropResTrue)

main :: IO ()
main = defaultMain (testGroup "Ltlspec" [testSimple])
