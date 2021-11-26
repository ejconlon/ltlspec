module Ltlspec.Test.Main (main) where

import Control.Monad (ap, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Functor (($>))
import qualified Data.Map.Strict as Map
import Ltlspec (envPropFold, propAlways, propEventually, propForAllNested, propIf, propIfNested)
import Ltlspec.Driver (DriverError (..), driveVerificationIO)
import Ltlspec.Models.Chat.Chat (chatTheory, longTrace, shortTrace)
import Ltlspec.Models.Ping.Actors (pingCase)
import Ltlspec.Models.Ping.Verification (PingWorld (PingWorld), emptyPingState, pingTheory, pingWorldsNotOk,
                                         pingWorldsOk)
import Ltlspec.System.Actors (ActorCase, AnnoMessage, runActorCaseSimple)
import Ltlspec.System.Logging (LogEntry, Logger, flushLogVar, newLogVar, varLogger)
import Ltlspec.System.Time (TimeDelta, timeDeltaFromFracSecs)
import Ltlspec.Types (ApplyAction (..), Atom (..), Binder (..), Bridge (..), EnvProp (..), EnvPropBad (..),
                      EnvPropGood (..), EnvPropRes, EnvPropStep (..), Prop (..), SAS, Theory (..), TruncBridge, VarName,
                      initScanSAS)
import System.Environment (lookupEnv, setEnv)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stderr, stdout)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, testCaseInfo, (@?=))

testCaseSkip :: Bool -> String -> IO () -> TestTree
testCaseSkip ci name body = testCaseInfo name (if ci then pure "SKIPPED" else body $> "")

-- | A tick interval for actor tests
shortTickInterval :: TimeDelta
shortTickInterval = timeDeltaFromFracSecs (0.01 :: Double)

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
  let (actualSteps, actualWorld, actualRes) = envPropFold (EnvProp mempty prop) worlds
  if actualSteps == 0
    then actualWorld @?= Nothing
    else actualWorld @?= Just (worlds !! (actualSteps - 1))
  (actualSteps, actualRes) @?= (expectedSteps, expectedRes)

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
  , EqCase "1 world; next1 #t" [emptyWorld] (PropNext PropTrue) 1 (Right (EnvPropGoodNext (EnvPropStepSingle (EnvProp mempty PropTrue))))
  , EqCase "2 world; not (next false) #t" [emptyWorld, emptyWorld] (PropNot (PropNext PropFalse)) 2 (Right (EnvPropGoodBool True))
  , EqCase "2 world; next (not false) #t" [emptyWorld, emptyWorld] (PropNext (PropNot PropFalse)) 2 (Right (EnvPropGoodBool True))
  , EqCase "2 world; not (and (next true) (next false))) #t" [emptyWorld, emptyWorld] (PropNot (PropAnd (PropNext PropTrue) (PropNext PropFalse))) 2 (Right (EnvPropGoodBool True))
  , EqCase "2 world; not (or (next false) (next false))) #t" [emptyWorld, emptyWorld] (PropNot (PropOr (PropNext PropFalse) (PropNext PropFalse))) 2 (Right (EnvPropGoodBool True))
  , EqCase "3 world; next2 #t" [emptyWorld, emptyWorld, emptyWorld] (PropNext PropTrue) 2 (Right (EnvPropGoodBool True))
  , EqCase "3 world; next3 #?" [emptyWorld, emptyWorld, emptyWorld] (PropNext (PropNext PropTrue)) 3 (Right (EnvPropGoodBool True))
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

newtype LogM a = LogM { unLogM :: Logger -> IO [LogEntry] -> IO a }
  deriving stock (Functor)

instance Applicative LogM where
  pure a = LogM (\_ _ -> pure a)
  (<*>) = ap

instance Monad LogM where
  return = pure
  LogM f >>= g = LogM $ \logger getLogEntries -> do
    a <- f logger getLogEntries
    let LogM h = g a
    h logger getLogEntries

instance MonadIO LogM where
  liftIO x = LogM (\_ _ -> x)

instance MonadFail LogM where
  fail = liftIO . fail

flushLogM :: LogM [LogEntry]
flushLogM = LogM (\_ getLogEntries -> getLogEntries)

loggerLogM :: LogM Logger
loggerLogM = LogM (\logger _ -> pure logger)

runLogM :: LogM a -> IO a
runLogM (LogM f) = do
  -- Use the following two lines intead of those after to log to the console:
  -- logger <- consoleLogger
  -- f logger (pure [])
  logVar <- newLogVar
  f (varLogger logVar) (flushLogVar logVar)

runDriverTest :: (TruncBridge e v w, Show e) => Theory -> [w] -> LogM (Either (DriverError e) ())
runDriverTest theory worlds = do
  logger <- loggerLogM
  liftIO (driveVerificationIO logger theory worlds)

assertDriverTestOk :: (TruncBridge e v w, Show e) => Theory -> [w] -> LogM ()
assertDriverTestOk theory worlds = do
  res <- runDriverTest theory worlds
  case res of
    Left err -> do
      logEntries <- flushLogM
      fail ("Failed to verify: " <> show err <> " | " <> show logEntries)
    _ -> pure ()

runActorTest :: ApplyAction (AnnoMessage a) s => ActorCase a -> s -> LogM [SAS s (AnnoMessage a)]
runActorTest kase initState = do
  logger <- loggerLogM
  actions <- liftIO (runActorCaseSimple logger kase)
  pure (initScanSAS applyAction initState actions)

testPingTraceOk :: TestTree
testPingTraceOk = testCase "Ping trace ok" (runLogM (assertDriverTestOk pingTheory pingWorldsOk))

testPingTraceNotOk :: TestTree
testPingTraceNotOk = testCase "Ping trace not ok" $ runLogM $ do
  res <- runDriverTest pingTheory pingWorldsNotOk
  case res of
    Left err ->
      case err of
        DriverErrorStepFalse _ -> pure ()
        _ -> do
          logEntries <- flushLogM
          fail ("Unexpected error: " <> show err <> " | " <> show logEntries)
    Right val -> fail ("Unexpected success: " <> show val)

testPingActors :: TestTree
testPingActors = testCase "Ping actors" $ runLogM $ do
  let limit = 5
  xs <- runActorTest (pingCase limit shortTickInterval) emptyPingState
  assertDriverTestOk pingTheory (fmap PingWorld xs)

testPing :: TestTree
testPing = testGroup "Ping"
  [ testPingTraceOk
  , testPingTraceNotOk
  , testPingActors
  ]

testChatShortTraceOk :: TestTree
testChatShortTraceOk = testCase "Chat short trace ok" (runLogM (assertDriverTestOk chatTheory shortTrace))

-- TODO figure out why this doesn't work well on CI
testChatLongTraceOk :: Bool -> TestTree
testChatLongTraceOk ci = testCaseSkip ci "Chat long trace ok" (runLogM (assertDriverTestOk chatTheory longTrace))

testChat :: Bool -> TestTree
testChat ci = testGroup "Chat"
  [ testChatShortTraceOk
  , testChatLongTraceOk ci
  ]

main :: IO ()
main = do
  mayDebugStr <- lookupEnv "DEBUG"
  mayCiStr <- lookupEnv "CI"
  let debug = Just "1" == mayDebugStr
      ci = Just "true" == mayCiStr
  when debug $ do
    setEnv "TASTY_NUM_THREADS" "1"
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering
  defaultMain $ testGroup "Ltlspec"
    [ testEqCases
    , testPing
    , testChat ci
    ]
