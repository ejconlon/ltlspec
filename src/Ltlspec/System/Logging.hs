module Ltlspec.System.Logging where

import Control.Concurrent.MVar (modifyMVarMasked_, newMVar)
import Data.IORef (IORef, modifyIORef', readIORef)
import Ltlspec.System.Time (currentMonoTime, monoTimeToNanos)

data LogLevel =
    LogLevelTrace
  | LogLevelDebug
  | LogLevelInfo
  | LogLevelWarn
  | LogLevelError
  deriving stock (Eq, Ord, Show, Enum, Bounded)

logLevelStr :: LogLevel -> String
logLevelStr = \case
  LogLevelTrace -> "TRACE"
  LogLevelDebug -> "DEBUG"
  LogLevelInfo -> "INFO"
  LogLevelWarn -> "WARN"
  LogLevelError -> "ERROR"

newtype Logger = Logger { runLogger :: LogLevel -> String -> IO () }

disabledLogger :: Logger
disabledLogger = Logger (\_ _ -> pure ())

filterLogger :: LogLevel -> Logger -> Logger
filterLogger ll0 (Logger f) = Logger (\ll1 s -> if ll1 >= ll0 then f ll1 s else pure ())

consoleLogger :: IO Logger
consoleLogger = do
  lock <- newMVar ()
  pure $ Logger $ \ll s ->
    modifyMVarMasked_ lock $ \_ -> do
      t <- fmap monoTimeToNanos currentMonoTime
      putStrLn ("[" ++ show t ++ "][" ++ logLevelStr ll ++ "] " ++ s)

type LogVar = IORef [(LogLevel, String)]

readLogVar :: LogVar -> IO [(LogLevel, String)]
readLogVar = fmap reverse . readIORef

varLogger :: LogVar -> Logger
varLogger lv = Logger (\ll s -> modifyIORef' lv ((ll, s):))
