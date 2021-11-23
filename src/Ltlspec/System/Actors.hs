{-# LANGUAGE DeriveAnyClass #-}

module Ltlspec.System.Actors
  ( ActorId
  , SeqNum
  , MessageId (..)
  , AppMessage (..)
  , NetMessage (..)
  , Handler
  , Behavior
  , TimerConfig (..)
  , ActorConstructor
  , LogEvent (..)
  , filterLogEvents
  , findActorsWhere
  , runActorSystem
  , runActorSystemSimple
  , TickMessage (..)
  , mkTickConfig
  , filterTickEvents
  , Message (..)
  ) where

import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)
import Control.Concurrent.STM (STM, atomically, retry)
import Control.Concurrent.STM.TQueue (TQueue, flushTQueue, isEmptyTQueue, newTQueueIO, tryReadTQueue, writeTQueue)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, stateTVar)
import Control.DeepSeq (NFData)
import Control.Monad (foldM)
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable (for)
import GHC.Generics (Generic)
import Ltlspec.System.Logging (LogLevel (..), Logger (..), consoleLogger)
import Ltlspec.System.TBarrier (TBarrier, newTBarrierIO, signalTBarrier, waitingTBarrier)
import Ltlspec.System.TEvent (TEvent, isSetTEvent, newTEventDelay, newTEventIO, setTEvent)
import Ltlspec.System.Time (TimeDelta, threadDelayDelta)

-- | Identifier for an actor
newtype ActorId = ActorId { unActorId :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Hashable, NFData)

-- | Identifier for an timer
newtype TimerId = TimerId { unTimerId :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Hashable, NFData)

-- | Message sequence number - only unique per-actor
newtype SeqNum = SeqNum { unSeqNum :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Hashable, NFData)

-- | Identifying information for a 'NetworkMessage'
data MessageId = MessageId
  { messageKeyAid :: !ActorId
  , messageKeySeqNum :: !SeqNum
  } deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Hashable, NFData)

-- | A message as seen from the application layer
data AppMessage msg = AppMessage
  { appMessageAid :: !ActorId
  -- ^ The sender or the receiver of this message (depending on the context)
  , appMessagePayload :: !msg
  } deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (NFData)

-- | A message as seen from the network layer
data NetMessage msg = NetMessage
  { netMessageId :: !MessageId
  , netMessageBody :: !(AppMessage msg)
  } deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (NFData)

-- | An application message handler
type Handler msg = AppMessage msg -> STM ()

-- | Given a handler for outgoing messages, handle incoming messages
type Behavior msg = Handler msg -> Handler msg

-- | Constructs timer configs and behavior for a given actor given information about that actor and others
type ActorConstructor r msg = [(ActorId, r)] -> ActorId -> r -> ([TimerConfig msg], Behavior msg)

-- | Filters the list of actor configs with the given predicate
findActorsWhere :: (r -> Bool) -> [(ActorId, r)] -> [(ActorId, r)]
findActorsWhere f pairs = [p | p@(_, r) <- pairs, f r]

-- | Configuration for a periodic timer
data TimerConfig msg = TimerConfig
  { timerConfigStartDelay :: !(Maybe TimeDelta)
  -- ^ Amount of time to delay first invocation
  , timerConfigPeriodicity :: !(Maybe (TimeDelta, Maybe Int))
  -- ^ If this is Just then we repeatedly invoke the timer every so often
  -- with an optional max invocations
  , timerConfigReceivers :: ![ActorId]
  -- ^ Destinations for this message on every invocation (should be non-empty)
  , timerConfigPayload :: !msg
  -- ^ Payload to send on every invocation
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

-- | Reads the next element of the queue unless the event signals that processing should terminate.
-- Nothing means we are done and cleanup should proceed. Just means continue processing.
readUnlessDone :: TEvent -> TQueue a -> STM (Maybe a)
readUnlessDone doneEvent queue = do
  isSet <- isSetTEvent doneEvent
  if isSet
    then pure Nothing
    else do
      ma <- tryReadTQueue queue
      maybe retry (pure . Just) ma

-- | Runs the given callback on each element of the queue until the event signals that processing should terminate
processUntilDone :: TEvent -> TQueue a -> (a -> STM ()) -> IO ()
processUntilDone doneEvent queue cb = go where
  go = do
    done <- atomically $ do
      ma <- readUnlessDone doneEvent queue
      maybe (pure True) (\a -> cb a $> False) ma
    if done
      then pure ()
      else go

-- | Allocate a new sequence number
allocSeqNum :: TVar SeqNum -> STM SeqNum
allocSeqNum snVar = stateTVar snVar (\m -> (m, m + 1))

-- | Log events
data LogEvent msg =
    LogEventUndeliverable !ActorId !MessageId
  -- ^ Emitted by network when it cannot deliver message (recv, mid)
  | LogEventDelivered !ActorId !MessageId
  -- ^ Emitted by network when it delivers message to actor (recv, mid)
  | LogEventReceived !(NetMessage msg)
  -- ^ Emitted by actor when it dequeues a correctly-delivered message from the queue
  | LogEventProcessed !ActorId !MessageId
  -- ^ Emitted by actor when it finishes processing a message from the queue (self, mid)
  | LogEventMisdelivered !ActorId !MessageId !ActorId
  -- ^ Emitted by actor when it dequeues an incorrectly-delivered message from the queue (self, mid, recv)
  | LogEventSent !(NetMessage msg)
  -- ^ Emitted by actor when it sends a message
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (NFData)

filterLogEvent :: (msg -> Maybe etc) -> Set MessageId -> LogEvent msg -> Maybe (LogEvent etc, Set MessageId)
filterLogEvent f s = \case
  LogEventUndeliverable recv mid ->
    if Set.member mid s then Just (LogEventUndeliverable recv mid, s) else Nothing
  LogEventDelivered recv mid ->
    if Set.member mid s then Just (LogEventDelivered recv mid, s) else Nothing
  LogEventReceived (NetMessage mid (AppMessage recv msg)) ->
    case f msg of
      Nothing -> Nothing
      Just etc ->
        let nm = NetMessage mid (AppMessage recv etc)
        in Just (LogEventReceived nm, Set.insert mid s)
  LogEventProcessed self mid ->
    if Set.member mid s then Just (LogEventProcessed self mid, s) else Nothing
  LogEventMisdelivered self mid recv ->
    if Set.member mid s then Just (LogEventMisdelivered self mid recv, s) else Nothing
  LogEventSent (NetMessage mid (AppMessage recv msg)) ->
    case f msg of
      Nothing -> Nothing
      Just etc ->
        let nm = NetMessage mid (AppMessage recv etc)
        in Just (LogEventSent nm, Set.insert mid s)

-- | Filter log events by message value (keeping relevant administrative messages)
filterLogEvents :: (msg -> Maybe etc) -> [LogEvent msg] -> [LogEvent etc]
filterLogEvents f = go Set.empty where
  go !s = \case
    [] -> []
    hd:tl ->
      case filterLogEvent f s hd of
        Nothing -> go s tl
        Just (hd', s') ->
          let tl' = go s' tl
          in hd':tl'

-- | The body of the network thread. Reads messages from the global queue and delivers them.
-- On termination flushes the log.
mkNetworkBody :: Logger -> TEvent -> TQueue (NetMessage msg) -> TQueue (LogEvent msg) -> Map ActorId (TQueue (NetMessage msg)) -> IO ()
mkNetworkBody logger doneEvent globalQueue logQueue actorQueues = do
  runLogger logger LogLevelDebug "network started"
  processUntilDone doneEvent globalQueue $ \netMsg@(NetMessage mid (AppMessage recvAid _)) -> do
    case Map.lookup recvAid actorQueues of
      Nothing -> writeTQueue logQueue (LogEventUndeliverable recvAid mid)
      Just actorQueue -> do
        writeTQueue logQueue (LogEventDelivered recvAid mid)
        writeTQueue actorQueue netMsg
  runLogger logger LogLevelDebug "network done processing"
  runLogger logger LogLevelDebug "network stopped"

-- | The body of the monitor thread. On quiescence sets the done event to stop all threads.
mkMonitorBody :: Logger -> TEvent -> TQueue (NetMessage msg) -> Map ActorId (TQueue (NetMessage msg)) -> TBarrier ->  TQueue (LogEvent msg) -> MVar [LogEvent msg] -> IO ()
mkMonitorBody logger doneEvent globalQueue actorQueues timerBarrier logQueue outVar = go where
  go = do
    runLogger logger LogLevelDebug "monitor started"
    atomically monitor
    runLogger logger LogLevelDebug "monitor flushing logs"
    logEvents <- atomically (flushTQueue logQueue)
    runLogger logger LogLevelDebug "monitor writing logs"
    putMVar outVar logEvents
    runLogger logger LogLevelDebug "monitor stopped"
  monitor = do
    isDone <- isSetTEvent doneEvent
    if isDone
      then pure ()
      else do
        isWaitingTimers <- waitingTBarrier timerBarrier
        if isWaitingTimers
          then retry
          else do
            isGlobalEmpty <- isEmptyTQueue globalQueue
            if isGlobalEmpty
              then do
                allActorsEmpty <- foldM (\e q -> if not e then pure False else isEmptyTQueue q) True (Map.elems actorQueues)
                if allActorsEmpty
                  then setTEvent doneEvent
                  else retry
              else retry

mkTimerBody :: Logger -> TimerId -> ActorId -> TimerConfig msg -> TEvent -> TBarrier -> Handler msg -> IO ()
mkTimerBody logger timerId sendAid (TimerConfig mayDelay mayPeriod recvAids pay) doneEvent timerBarrier netHandler = go where
  tstr = "timer " ++ show (unTimerId timerId) ++ " (actor " ++ show (unActorId sendAid) ++ ")"
  go = do
    runLogger logger LogLevelDebug $ tstr ++ " started"
    -- Delay initially if configured to do so
    case mayDelay of
      Nothing -> pure ()
      Just delay -> do
        runLogger logger LogLevelDebug $ tstr ++ " initial delay"
        threadDelayDelta delay
    -- Enter the timer loop
    recur 0
    runLogger logger LogLevelDebug $ tstr ++ " signaling"
    atomically (signalTBarrier timerBarrier)
    runLogger logger LogLevelDebug $ tstr ++ " stopped"
  recur !count = do
    runLogger logger LogLevelDebug $ tstr ++ " invoking"
    -- Loop until done - first check done event and send
    eventDone <- atomically $ do
      isDone <- isSetTEvent doneEvent
      if isDone
        then pure True
        else do
          for_ recvAids $ \recvAid -> do
            let appMsg = AppMessage recvAid pay
            netHandler appMsg
          pure False
    if eventDone
      then pure ()
      else do
        -- Delay for next interval if necessary
        periodDone <- do
          case mayPeriod of
            Nothing -> pure True
            Just (interval, mayLim) -> do
              case mayLim of
                Just lim | lim <= count -> pure True
                _ -> do
                  runLogger logger LogLevelDebug $ tstr ++ " periodic delay"
                  threadDelayDelta interval $> False
        if periodDone
          then pure ()
          else recur (count + 1)

mkNetworkHandler :: ActorId -> TVar SeqNum -> TQueue (NetMessage msg) -> TQueue (LogEvent msg) -> Handler msg
mkNetworkHandler sendAid snVar globalQueue logQueue appMsg = do
  sn <- allocSeqNum snVar
  let mid = MessageId sendAid sn
      netMsg = NetMessage mid appMsg
  writeTQueue globalQueue netMsg
  writeTQueue logQueue (LogEventSent netMsg)

mkActorBody :: Logger -> ActorId -> TEvent -> TQueue (NetMessage msg) -> TQueue (LogEvent msg) -> Handler msg -> IO ()
mkActorBody logger aid doneEvent actorQueue logQueue handler = do
  let astr = "actor " ++ show (unActorId aid)
  runLogger logger LogLevelDebug $ astr ++ " started"
  processUntilDone doneEvent actorQueue $ \nm@(NetMessage mid appMsg@(AppMessage recvAid _)) -> do
    if aid == recvAid
      then do
        writeTQueue logQueue (LogEventReceived nm)
        handler appMsg
        writeTQueue logQueue (LogEventProcessed aid mid)
      else writeTQueue logQueue (LogEventMisdelivered aid mid recvAid)
  runLogger logger LogLevelDebug $ astr ++ " stopped"

-- | Run the actor system
runActorSystem :: Logger -> Maybe TimeDelta -> ActorConstructor r msg -> [r] -> IO (TEvent, [ThreadId], MVar [LogEvent msg])
runActorSystem logger mayLimit ctor configs = do
  -- Assign actor ids
  let configPairs = zip (fmap ActorId [0..]) configs
  -- Alocate queues, vars, and events
  globalQueue <- newTQueueIO
  logQueue <- newTQueueIO
  outVar <- newEmptyMVar
  actorQueues <- foldM (\m aid -> newTQueueIO >>= \q -> pure (Map.insert aid q m)) Map.empty (fmap fst configPairs)
  actorSnVars <- foldM (\m aid -> newTVarIO 0 >>= \v -> pure (Map.insert aid v m)) Map.empty (fmap fst configPairs)
  doneEvent <- maybe newTEventIO newTEventDelay mayLimit
  -- Construct actors and timers
  let ctorResults = flip fmap configPairs $ \(aid, config) ->
        let (tconfigs, behavior) = ctor configPairs aid config
        in (aid, filter (not . null . timerConfigReceivers) tconfigs, behavior)
  let timerSet = zip (fmap TimerId [0..]) (ctorResults >>= \(aid, tconfigs, _) -> fmap (aid,) tconfigs)
  let actorSet = fmap (\(aid, _, behavior) -> (aid, behavior)) ctorResults
  -- Now that we know how many timers, create a barrier of that size
  timerBarrier <- newTBarrierIO (length timerSet)
  -- Spawn threads
  let networkBody = mkNetworkBody logger doneEvent globalQueue logQueue actorQueues
      monitorBody = mkMonitorBody logger doneEvent globalQueue actorQueues timerBarrier logQueue outVar
  networkThreadId <- forkIO networkBody
  monitorThreadId <- forkIO monitorBody
  timerThreadIds <- for timerSet $ \(tid, (aid, tconfig)) ->
    let snVar = actorSnVars Map.! aid
        networkHandler = mkNetworkHandler aid snVar globalQueue logQueue
        timerBody = mkTimerBody logger tid aid tconfig doneEvent timerBarrier networkHandler
    in forkIO timerBody
  actorThreadIds <- for actorSet $ \(aid, behavior) ->
    let snVar = actorSnVars Map.! aid
        actorQueue = actorQueues Map.! aid
        networkHandler = mkNetworkHandler aid snVar globalQueue logQueue
        handler = behavior networkHandler
        actorBody = mkActorBody logger aid doneEvent actorQueue logQueue handler
    in forkIO actorBody
  let allThreadIds = [networkThreadId, monitorThreadId] ++ timerThreadIds ++ actorThreadIds
  pure (doneEvent, allThreadIds, outVar)

-- | A simpler version - NOTE this only works for systems that go quiescent!
-- Otherwise will wait forever...
runActorSystemSimple :: ActorConstructor r msg -> [r] -> IO [LogEvent msg]
runActorSystemSimple ctor configs = do
  logger <- consoleLogger
  (_, _, outVar) <- runActorSystem logger Nothing ctor configs
  readMVar outVar

-- | Wraps messages to add clock tick events
data TickMessage msg =
    TickMessageFire
  | TickMessageEmbed !msg
  deriving stock (Eq, Show)

-- | Makes a timer config for a tick message fire
mkTickConfig :: Maybe TimeDelta -> TimeDelta -> Maybe Int -> ActorId -> TimerConfig (TickMessage msg)
mkTickConfig mayDelay interval mayLimit recvAid =
  TimerConfig mayDelay (Just (interval, mayLimit)) [recvAid] TickMessageFire

-- | Keeps only the embedded (non-fire) events
filterTickEvents :: [LogEvent (TickMessage msg)] -> [LogEvent msg]
filterTickEvents = filterLogEvents (\case { TickMessageFire -> Nothing; TickMessageEmbed msg -> Just msg })

-- | A simple message triple (sender, receiver, payload) to use in verification.
data Message msg = Message
  { messageSender :: !ActorId
  , messageReceiver :: !ActorId
  , messagePayload :: !msg
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Extracts a simple message triple from a log events
extractMessage :: LogEvent msg -> Maybe (Message msg)
extractMessage = \case
  LogEventReceived (NetMessage (MessageId sendAid _) (AppMessage recvAid msg)) ->
    Just (Message sendAid recvAid msg)
  _ -> Nothing

-- | Keep only message receive events in a simple format.
filterMessages :: [LogEvent msg] -> [Message msg]
filterMessages = mapMaybe extractMessage
