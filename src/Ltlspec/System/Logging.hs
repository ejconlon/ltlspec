module Ltlspec.System.Logging
  ( LogLevel (..)
  , logLevelToStr
  , strToLogLevel
  , Logger (..)
  , disabledLogger
  , filterLogger
  , consoleLogger
  , LogEntry (..)
  , LogVar
  , newLogVar
  , flushLogVar
  , varLogger
  ) where

import Control.Concurrent.MVar (MVar, modifyMVarMasked_, newMVar, swapMVar)
import Ltlspec.System.Time (MonoTime, currentMonoTime, monoTimeToNanos)

-- | Log level... TRACE is most verbose, ERROR is least
data LogLevel =
    LogLevelTrace
  | LogLevelDebug
  | LogLevelInfo
  | LogLevelWarn
  | LogLevelError
  deriving stock (Eq, Ord, Show, Enum, Bounded)

logLevelToStr :: LogLevel -> String
logLevelToStr = \case
  LogLevelTrace -> "TRACE"
  LogLevelDebug -> "DEBUG"
  LogLevelInfo -> "INFO"
  LogLevelWarn -> "WARN"
  LogLevelError -> "ERROR"

strToLogLevel :: String -> Maybe LogLevel
strToLogLevel = \case
  "TRACE" -> Just LogLevelTrace
  "DEBUG" -> Just LogLevelDebug
  "INFO" -> Just LogLevelInfo
  "WARN" -> Just LogLevelWarn
  "ERROR" -> Just LogLevelError
  _ -> Nothing

-- | A Logger is any action given a log level and message.
-- Use like `runLogger logger LogLevelInfo "hello"`
newtype Logger = Logger { runLogger :: LogLevel -> String -> IO () }

-- | A logger that does nothing.
disabledLogger :: Logger
disabledLogger = Logger (\_ _ -> pure ())

-- | Change the allowed log level with this. `filterLogger LogLevelWarn`
-- will allow WARN and ERROR messages but no others.
-- `filterLogger LogLevelTrace` is a no-op
filterLogger :: LogLevel -> Logger -> Logger
filterLogger ll0 logger =
  case ll0 of
    LogLevelTrace -> logger
    _ ->Logger (\ll1 s -> if ll1 >= ll0 then runLogger logger ll1 s else pure ())

-- | A printf logger that LOCKS to avoid concurrent access to stdout.
-- Note that other threads may still print and make things ugly.
consoleLogger :: IO Logger
consoleLogger = do
  lock <- newMVar ()
  pure $ Logger $ \ll s ->
    modifyMVarMasked_ lock $ \_ -> do
      t <- fmap monoTimeToNanos currentMonoTime
      putStrLn ("[" ++ show t ++ "][" ++ logLevelToStr ll ++ "] " ++ s)

-- | A logger invocation as data.
data LogEntry = LogEntry
  { logEntryTime :: !MonoTime
  , logEntryLevel :: !LogLevel
  , logEntryMessage :: !String
  } deriving stock (Eq, Show)

-- | A place to store log entries
type LogVar = MVar [LogEntry]

-- | Creates a new log var
newLogVar :: IO LogVar
newLogVar = newMVar []

-- | Flushes the log var to a list (returning in time order)
flushLogVar :: LogVar -> IO [LogEntry]
flushLogVar lv = fmap reverse (swapMVar lv [])

-- | A logger that writes to a list in memory. (Locks around access.)
-- Useful for testing logging itself.
varLogger :: LogVar -> Logger
varLogger lv = Logger $ \ll s ->
  modifyMVarMasked_ lv $ \tl -> do
    t <- currentMonoTime
    let e = LogEntry t ll s
    pure (e:tl)
