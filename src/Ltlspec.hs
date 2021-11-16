-- | Linear Temporal Logic (LTL) propositions and functions for manipulating and evaluating them.
module Ltlspec where

import Control.Monad.Writer.Strict (execWriter, tell)
import Data.Functor.Foldable (embed, fold, project)
import qualified Data.Map.Strict as M
import Data.Semigroup (Max (..), Sum (..))
import Data.Sequence (Seq (..), fromList)
import Ltlspec.Recursion (foldUpM)
import Ltlspec.Types (Atom (..), AtomVar, Binder (..), Bridge (..), Env, EnvProp (..), EnvPropBad (..),
                      EnvPropGood (..), EnvPropRes, EnvPropStep (..), Prop (..), PropF (..), PropName, Quantifier (..),
                      SAS (..), TyName, VarName)

-- | Scan a list of actions into a list of SAS
scanSAS :: (a -> s -> s) -> s -> [a] -> [SAS s a]
scanSAS update initState actions = result where
  result = case actions  of
    [] -> []
    a:as -> scanr go (initWorld a) as
  initWorld a = SAS initState a (update a initState)
  go a (SAS _ _ after) = SAS after a (update a after)

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
lookupEnvName zs x = M.lookup x zs

-- We should report an error for the case below
-- case2: (∀a:T.(∀a:R. p(a)))
insertEnv :: Env v -> VarName -> v -> Env v
insertEnv env name val = M.insertWithKey (\k _ -> error ("Data variable " ++ k ++ " was bound twice! (during insertion)")) name val env

-- | Looks up all atom variables in the environment ('Left' means missing)
lookupEnvAtom :: Env v -> AtomVar -> Either VarName (Atom v)
lookupEnvAtom env = traverse (\n -> maybe (Left n) Right (lookupEnvName env n))

-- | Combines "forall" branch results
sequenceForAllRes :: [EnvPropRes e v] -> EnvPropRes e v
sequenceForAllRes rs = sequenceA rs >>= go Empty where
  go !acc = \case
    [] -> case acc of
      -- Acc will be empty when all prop eval results are True
      -- Thus the ForAll prop is True
      Empty -> Right (EnvPropGoodBool True)
      _ -> Right (EnvPropGoodNext (EnvPropStepParallel QuantifierForAll acc))
    p:ps ->
      case p of
        EnvPropGoodBool b -> if b then go acc ps else Right p
        EnvPropGoodNext x -> go (acc :|> x) ps

-- | Combines "exists" branch results
sequenceExistsRes :: [EnvPropRes e v] -> EnvPropRes e v
sequenceExistsRes rs = sequenceA rs >>= go Empty where
  go !acc = \case
    [] -> case acc of
      -- Similarly, acc will be empty when all prop eval results are False
      -- Thus the Exists prop is False
      Empty -> Right (EnvPropGoodBool False)
      _ -> Right (EnvPropGoodNext (EnvPropStepParallel QuantifierExists acc))
    p:ps ->
      case p of
        EnvPropGoodBool b -> if b then Right p else go acc ps
        EnvPropGoodNext x -> go (acc :|> x) ps

-- typing less is a blessing
pattern GoodB :: Bool -> Either a (EnvPropGood v)
pattern GoodB b = Right (EnvPropGoodBool b)

pattern GoodN :: EnvPropStep v -> Either a (EnvPropGood v)
pattern GoodN ep = Right (EnvPropGoodNext ep)

data PropCombinator = PcAnd | PcOr

-- | Anytime we have And/Or/Forall/Exists, we need to check multiple propostions concurrently
-- This merge function will merge single step propositions into a sequence.
-- The elements in the sequence are *conceptually* running in parallel.
mergeEnvPropSteps :: PropCombinator -> EnvPropStep v -> EnvPropStep v -> EnvPropStep v
mergeEnvPropSteps pc step1 step2 = combine pc step1 step2
  where
    combine combinator s1 s2 = case combinator of
      PcAnd -> EnvPropStepParallel QuantifierForAll (fromList [s1, s2])
      PcOr -> EnvPropStepParallel QuantifierExists (fromList [s1, s2])

negateEnvPropStep :: EnvPropStep v -> EnvPropStep v
negateEnvPropStep = \case
  EnvPropStepSingle (EnvProp e p) -> EnvPropStepSingle (EnvProp e (PropNot p))
  EnvPropStepParallel qt eps ->
    let eps' = fmap negateEnvPropStep eps
    in EnvPropStepParallel qt eps'

-- TODO(yanze) implement this and resurrect unit tests!
envPropEval :: Bridge e v w => EnvProp v -> w -> EnvPropRes e v
envPropEval ep0@(EnvProp env0 prop0) world = go env0 prop0 where
  go env prop =
    case prop of
      PropAtom atomVar ->
        case lookupEnvAtom env atomVar of
          Left varName -> Left (EnvPropBadMissing varName)
          Right atomVal ->
            case bridgeEvalProp world atomVal of
              Left err -> Left (EnvPropBadErr err)
              Right anotherProp -> go env anotherProp
      PropTrue -> GoodB True
      PropFalse -> GoodB False
      PropNot p -> case go env p of
        bad@(Left _) -> bad
        Right (EnvPropGoodBool True) -> GoodB False
        Right (EnvPropGoodBool False) -> GoodB True
        Right (EnvPropGoodNext next) -> GoodN $ negateEnvPropStep next
      PropAnd p1 p2 -> case go env p1 of
        -- the evaluation of p1 raised an error
        bad@(Left _) -> bad
        -- p1 is true, so we can safely discard it
        Right (EnvPropGoodBool True) -> go env p2
        -- p1 is false, so the whole prop is false
        false@(Right (EnvPropGoodBool False)) -> false
        -- p1 needs evaluation in the next world, now hanlde p2
        Right (EnvPropGoodNext next1) -> case go env p2 of
          -- the evaluation of p1 raised an error
          bad@(Left _) -> bad
          -- p2 is true, so we only need to evaluate the residual part of p1 in the next world
          Right (EnvPropGoodBool True) -> GoodN next1
          -- p2 is false, so the whole prop is false
          false@(Right (EnvPropGoodBool False)) -> false
          -- both p1 and p2 needs further evaluation in the next world,
          -- merge them based on the current proposition (PropAnd)
          Right (EnvPropGoodNext next2) -> GoodN $ mergeEnvPropSteps PcAnd next1 next2
      PropOr p1 p2 -> case go env p1 of
        bad@(Left _) -> bad
        Right (EnvPropGoodBool False) -> go env p2
        true@(Right (EnvPropGoodBool True)) -> true
        Right (EnvPropGoodNext next1) -> case go env p2 of
          bad@(Left _) -> bad
          Right (EnvPropGoodBool False) -> GoodN next1
          true@(Right (EnvPropGoodBool True)) -> true
          Right (EnvPropGoodNext next2) -> GoodN $ mergeEnvPropSteps PcOr next1 next2
      PropNext p -> GoodN $ EnvPropStepSingle (EnvProp env p)
      PropUntil p1 p2 -> case go env p2 of
        bad@(Left _) -> bad
        -- p2 is true, the prop is true!
        true@(Right (EnvPropGoodBool True)) -> true
        -- p2 is false, we need to check if p1 holds
        Right (EnvPropGoodBool False) -> case go env p1 of
          bad@(Left _) -> bad
          -- p1 is true, so we need to check the prop against the next world
          Right (EnvPropGoodBool True) -> GoodN $ EnvPropStepSingle ep0
          -- p1 is false, the whole prop fails
          false@(Right (EnvPropGoodBool False)) -> false
          -- p1 has some residual for next world
          -- The residual has to be true (in the next world) for the prop to not fail in the *current* world.
          -- Therefore we use PropAnd to connect the residual and the original prop.
          -- the residual will be evaluate first, and if it's true, the proposition is proven;
          -- otherwise, we need to evaluate the same prop in the next world (next time tick)
          Right (EnvPropGoodNext next) -> GoodN $ mergeEnvPropSteps PcAnd next (EnvPropStepSingle ep0)
        -- p2 has some residual for next world
        -- If p2 is evaluate to be true in the next world,
        -- the whole prop is true in the *current* world.
        -- Therefore, we use PropOr to connect.
        Right (EnvPropGoodNext next) -> GoodN $ mergeEnvPropSteps PcOr next (EnvPropStepSingle ep0)
      PropRelease p1 p2 -> case go env p2 of
        bad@(Left _) -> bad
        -- p2 is false, the prop is false!
        false@(Right (EnvPropGoodBool False)) -> false
        -- p2 is true, we need to check if p1 holds
        Right (EnvPropGoodBool True) -> case go env p1 of
          bad@(Left _) -> bad
          -- p1 is true, the whole prop is true
          true@(Right (EnvPropGoodBool True)) -> true
          -- p1 is false, thus p2 cannot be released
          -- the show must go on
          Right (EnvPropGoodBool False) -> GoodN $ (EnvPropStepSingle ep0)
          -- p1 has some residual for next world
          -- If the residual is true, the prop is proven,
          -- otherwise we need to keep evaluating the prop in next world
          -- Therefore we use PropOr to connect the residual and the original prop.
          Right (EnvPropGoodNext next) -> GoodN $ mergeEnvPropSteps PcOr next (EnvPropStepSingle ep0)
        -- Similarly, here we use PropAnd to connect
        Right (EnvPropGoodNext next) -> GoodN $ mergeEnvPropSteps PcAnd next (EnvPropStepSingle ep0)
      PropForAll (Binder varName tyName) bodyProp ->
        case bridgeQuantify world tyName of
          Left err -> Left (EnvPropBadErr err)
          Right vals ->
            let results = fmap (\val -> go (insertEnv env varName val) bodyProp) vals
            in sequenceForAllRes results
      PropExists (Binder varName tyName) bodyProp ->
        case bridgeQuantify world tyName of
          Left err -> Left (EnvPropBadErr err)
          Right vals ->
            let results = fmap (\val -> go (insertEnv env varName val) bodyProp) vals
            in sequenceExistsRes results

evalEnvPropGood :: Bridge e v w => EnvPropGood v -> w -> EnvPropRes e v
evalEnvPropGood res world = case res of
  EnvPropGoodBool _ -> Right res
  EnvPropGoodNext (EnvPropStepSingle p) -> envPropEval p world
  EnvPropGoodNext (EnvPropStepParallel qt ps) ->
    let
      allRes = fmap (\x -> evalEnvPropGood (EnvPropGoodNext x) world) ps
      results = foldr (\r rs -> r : rs) [] allRes
      result = case qt of
        QuantifierForAll -> sequenceForAllRes results
        QuantifierExists -> sequenceExistsRes results
    in result

-- | Evaluate the prop at every timestep until true/false or there are no more inputs.
-- Also returns the number of timesteps evaluated.
-- NOTE: Worlds start from the leftmost one in the list
envPropFold :: Bridge e v w => EnvProp v -> [w] -> (Int, EnvPropRes e v)
envPropFold p = go 0 (Right (EnvPropGoodNext (EnvPropStepSingle p))) where
  go i r [] = (i, r)
  go i r (w:ws) =
    case r of
      Left _ -> (i, r)
      Right (EnvPropGoodBool _) -> (i, r)
      Right good -> go (i+1) (evalEnvPropGood good w) ws
