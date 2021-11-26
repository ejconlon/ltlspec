module Ltlspec.Driver where

import Control.Exception (Exception)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Foldable (for_)
import qualified Data.Map.Strict as Map
import Data.Typeable (Typeable)
import Ltlspec (envPropFold, truncEnvPropStep)
import Ltlspec.System.Logging (Logger, logDebug)
import Ltlspec.TriBool (TriBool (..))
import Ltlspec.Types (EnvProp (..), EnvPropBad, EnvPropGood (..), Theory (..), TruncBridge)
-- import Text.Pretty.Simple (pPrint)

data DriverError e =
    DriverErrorBad !Int !(EnvPropBad e)
  | DriverErrorFalse !Int
  | DriverErrorStepEmpty
  | DriverErrorStepBad !Int !(EnvPropBad e)
  | DriverErrorStepFalse !Int
  | DriverErrorStepUnknown !Int
  deriving stock (Eq, Show)

instance (Show e, Typeable e) => Exception (DriverError e)

newtype DriverM e a = DriverM { unDriverM :: ExceptT (DriverError e) IO a }
  deriving newtype (Functor, Applicative, Monad, MonadError (DriverError e), MonadIO)

runDriverM :: DriverM e a -> IO (Either (DriverError e) a)
runDriverM = runExceptT . unDriverM

driveVerification :: (TruncBridge e v w, Show e, Show v) => Logger -> Theory -> [w] -> DriverM e ()
driveVerification logger theory trace = do
  for_ (Map.toList (theoryAxioms theory)) $ \(axName, axProp) -> do
    logDebug logger ("Verifying " <> axName)
    let initEnvProp = EnvProp mempty axProp
    let (i, mw, res) = envPropFold initEnvProp trace
    case res of
      Left bad -> do
        logDebug logger ("Bad result on world " <> show i <> ": " <> show bad)
        throwError (DriverErrorBad i bad)
      Right val -> case val of
        EnvPropGoodBool bval ->
          if bval
            then logDebug logger "Verified"
            else do
              logDebug logger "Falsified"
              throwError (DriverErrorFalse i)
        EnvPropGoodNext step -> do
          logDebug logger ("Truncating after world " <> show i)
          -- Uncomment to see an infinite loop...
          -- liftIO (pPrint step)
          case mw of
            Nothing -> do
              logDebug logger "No world to evaluate truncation"
              throwError DriverErrorStepEmpty
            Just w -> do
              case truncEnvPropStep w step of
                Left bad -> do
                  logDebug logger ("Bad truncation on world " <> show i <> ": " <> show bad)
                  throwError (DriverErrorStepBad i bad)
                Right tb -> case tb of
                  TriBoolTrue ->
                    logDebug logger "Verified by truncation"
                  TriBoolFalse -> do
                    logDebug logger "Falsified by truncation"
                    throwError (DriverErrorStepFalse i)
                  TriBoolUnknown -> do
                    logDebug logger "Truncation inconclusive"
                    throwError (DriverErrorStepUnknown i)

driveVerificationIO :: (TruncBridge e v w, Show e, Show v) => Logger -> Theory -> [w] -> IO (Either (DriverError e) ())
driveVerificationIO logger theory trace = runDriverM (driveVerification logger theory trace)
