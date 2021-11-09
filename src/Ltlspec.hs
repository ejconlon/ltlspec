-- | Linear Temporal Logic (LTL) propositions and functions for manipulating and evaluating them.
module Ltlspec where

import Control.Monad.Writer.Strict (execWriter, tell)
import Data.Functor.Foldable (embed, fold, project)
import Data.Semigroup (Max (..), Sum (..))
import Data.Sequence (Seq (..))
import Ltlspec.Recursion (foldUpM)
import Ltlspec.Types (Atom (..), AtomVar, Binder (..), Bridge (..), Env, EnvProp (..), EnvPropBad (..),
                      EnvPropGood (..), EnvPropRes, EnvPropStep (..), Prop (..), PropF (..), PropName, Quantifier (..),
                      SAS (..), TyName, VarName)

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
propAtoms :: Prop -> [AtomVar]
propAtoms = execWriter . foldUpM go where
  go = \case
    PropAtomF a -> tell [a]
    _ -> pure ()

-- | Looks up a variable in the environment ('Nothing' means missing)
lookupEnvName :: Env v -> VarName -> Maybe v
lookupEnvName zs x =
  case zs of
    Empty -> Nothing
    (n, v) :<| ys -> if x == n then Just v else lookupEnvName ys x

-- | Looks up all atom variables in the environment ('Left' means missing)
lookupEnvAtom :: Env v -> Atom VarName -> Either VarName (Atom v)
lookupEnvAtom env = traverse (\n -> maybe (Left n) Right (lookupEnvName env n))

-- | Combines "forall" branch results
sequenceForAllRes :: [EnvPropRes e v] -> Either (EnvPropBad e) (EnvPropGood v)
sequenceForAllRes rs = sequenceA rs >>= go Empty where
  go !acc = \case
    [] -> Right (EnvPropGoodNext (EnvPropStepQuant QuantifierForAll acc))
    p:ps ->
      case p of
        EnvPropGoodBool b -> if b then go acc ps else Right p
        EnvPropGoodNext x -> go (acc :|> x) ps

-- | Combines "exists" branch results
sequenceExistsRes :: [EnvPropRes e v] -> Either (EnvPropBad e) (EnvPropGood v)
sequenceExistsRes = error "TODO"

-- TODO(yanze) implement this and resurrect unit tests!
envPropEval :: Bridge e v w => EnvProp v -> w -> EnvPropRes e v
envPropEval (EnvProp env0 prop0) world = go env0 prop0 where
  go env prop =
    case prop of
      PropAtom atomVar ->
        case lookupEnvAtom env atomVar of
          Left varName -> Left (EnvPropBadMissing varName)
          Right atomVal ->
            case bridgeEvalProp world atomVal of
              Left err -> Left (EnvPropBadErr err)
              Right anotherProp -> go env anotherProp
      PropTrue -> Right (EnvPropGoodBool True)
      PropFalse -> Right (EnvPropGoodBool False)
      PropForAll (Binder varName tyName) bodyProp ->
        case bridgeQuantify world tyName of
          Left err -> Left (EnvPropBadErr err)
          Right vals ->
            let results = fmap (\val -> go ((varName,val) :<| env) bodyProp) vals
            in sequenceForAllRes results
      _ -> error "TODO"

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

-- | Evaluate the prop at every timestep until true/false or there are no more inputs.
-- Also returns the number of timesteps evaluated.
envPropFold :: Bridge e v w => EnvProp v -> [w] -> (Int, EnvPropRes e v)
envPropFold = go 0 where
  go i p ws =
    case ws of
      [] -> (i, Right (EnvPropGoodNext (EnvPropStepSingle p)))
      w:_ ->
        let i' = i + 1
            r = envPropEval p w
        in case r of
          Left _ -> (i', r)
          Right q ->
            case q of
              EnvPropGoodBool _ -> (i', r)
              EnvPropGoodNext _ -> error "TODO"

-- | Scan a list of actions into a list of SAS
scanSAS :: (a -> s -> s) -> s -> [a] -> [SAS s a]
scanSAS update initState actions = result where
  result = case actions  of
    [] -> []
    a:as -> scanr go (initWorld a) as
  initWorld a = SAS initState a (update a initState)
  go a (SAS _ _ after) = SAS after a (update a after)
