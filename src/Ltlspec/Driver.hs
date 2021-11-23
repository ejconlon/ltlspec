module Ltlspec.Driver where

import Control.Exception (Exception)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (for_)
import qualified Data.Map.Strict as Map
import Data.Typeable (Typeable)
import Ltlspec (envPropFold)
import Ltlspec.System.Logging (LogLevel (..), Logger, runLogger)
import Ltlspec.Types (Bridge, EnvProp (..), EnvPropBad, EnvPropGood (..), Theory (..))

data DriverError e =
    DriverErrorBad !Int !(EnvPropBad e)
  | DriverErrorFalse !Int
  deriving stock (Eq, Show)

instance (Show e, Typeable e) => Exception (DriverError e)

newtype DriverM e a = DriverM { unDriverM :: ExceptT (DriverError e) IO a }
  deriving newtype (Functor, Applicative, Monad, MonadError (DriverError e), MonadIO)

runDriverM :: DriverM e a -> IO (Either (DriverError e) a)
runDriverM = runExceptT . unDriverM

driveVerification :: (Bridge e v w, Show e) => Logger -> Theory -> [w] -> DriverM e ()
driveVerification logger theory trace = do
  for_ (Map.toList (theoryAxioms theory)) $ \(axName, axProp) -> do
    runLogger logger LogLevelDebug ("Verifying " <> axName)
    let initEnvProp = EnvProp mempty axProp
    let (i, res) = envPropFold initEnvProp trace
    case res of
      Left bad -> do
        runLogger logger LogLevelDebug ("Bad result on world " <> show i <> ": " <> show bad)
        throwError (DriverErrorBad i bad)
      Right val -> case val of
        EnvPropGoodBool bval ->
          if bval
            then runLogger logger LogLevelDebug "Verified"
            else do
              runLogger logger LogLevelDebug "Falsified"
              throwError (DriverErrorFalse i)
        EnvPropGoodNext _ ->
          runLogger logger LogLevelDebug ("Could not verify past world " <> show i)

driveVerificationIO :: (Bridge e v w, Show e) => Logger -> Theory -> [w] -> IO (Either (DriverError e) ())
driveVerificationIO logger theory trace = runDriverM (driveVerification logger theory trace)
