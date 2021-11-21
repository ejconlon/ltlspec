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
  , findActorsWhere
  , runActorSystem
  ) where

import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar)
import Control.Concurrent.STM (STM, retry)
import Control.Concurrent.STM.TQueue (TQueue, flushTQueue, isEmptyTQueue, newTQueueIO, tryReadTQueue, writeTQueue)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, stateTVar)
import Control.Monad (foldM)
import Control.Monad.STM (atomically)
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Traversable (for)
import Ltlspec.System.TBarrier (TBarrier, newTBarrierIO, signalTBarrier, waitingTBarrier)
import Ltlspec.System.TEvent (TEvent, isSetTEvent, newTEventDelay, newTEventIO, setTEvent)
import Ltlspec.System.Time (TimeDelta, threadDelayDelta)

-- | Identifier for an actor
newtype ActorId = ActorId { unActorId :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num)

-- | Message sequence number - only unique per-actor
newtype SeqNum = SeqNum { unSeqNum :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num)

-- | Identifying information for a 'NetworkMessage'
data MessageId = MessageId
  { messageKeyAid :: !ActorId
  , messageKeySeqNum :: !SeqNum
  } deriving stock (Eq, Show)

-- | A message as seen from the application layer
data AppMessage msg = AppMessage
  { appMessageAid :: !ActorId
  -- ^ The sender or the receiver of this message (depending on the context)
  , appMessagePayload :: !msg
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

-- | A message as seen from the network layer
data NetMessage msg = NetMessage
  { netMessageId :: !MessageId
  , netMessageBody :: !(AppMessage msg)
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

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
    LogEventUndeliverable !(NetMessage msg)
  | LogEventDelivered !(NetMessage msg)
  | LogEventProcessed !ActorId !MessageId
  | LogEventMisdelivered !ActorId !MessageId !ActorId
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

-- | The body of the network thread. Reads messages from the global queue and delivers them.
-- On termination flushes the log.
networkBody :: TEvent -> TQueue (NetMessage msg) -> TQueue (LogEvent msg) -> Map ActorId (TQueue (NetMessage msg)) -> MVar [LogEvent msg] -> IO ()
networkBody doneEvent globalQueue logQueue actorQueues outVar = do
  processUntilDone doneEvent globalQueue $ \netMsg@(NetMessage _ (AppMessage recvAid _)) -> do
    case Map.lookup recvAid actorQueues of
      Nothing -> writeTQueue logQueue (LogEventUndeliverable netMsg)
      Just actorQueue -> do
        writeTQueue logQueue (LogEventDelivered netMsg)
        writeTQueue actorQueue netMsg
  logEvents <- atomically (flushTQueue logQueue)
  putMVar outVar logEvents

-- | The body of the monitor thread. On quiescence sets the done event to stop all threads.
monitorBody :: TEvent -> TQueue (NetMessage msg) -> Map ActorId (TQueue (NetMessage msg)) -> TBarrier -> IO ()
monitorBody doneEvent globalQueue actorQueues timerBarrier = atomically $ do
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

timerBody :: ActorId -> TimerConfig msg -> TEvent -> TVar SeqNum -> TBarrier -> TQueue (NetMessage msg) -> IO ()
timerBody sendAid (TimerConfig mayDelay mayPeriod recvAids pay) doneEvent snVar timerBarrier globalQueue = go where
  go = do
    -- Delay initially
    maybe (pure ()) threadDelayDelta mayDelay
    recur 0
    atomically (signalTBarrier timerBarrier)
  recur !count = do
    -- Loop until done - first check done event and send
    eventDone <- atomically $ do
      isDone <- isSetTEvent doneEvent
      if isDone
        then pure True
        else do
          for_ recvAids $ \recvAid -> do
            sn <- allocSeqNum snVar
            let mid = MessageId sendAid sn
                appMsg = AppMessage recvAid pay
                netMsg = NetMessage mid appMsg
            writeTQueue globalQueue netMsg
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
                _ -> threadDelayDelta interval $> False
        if periodDone
          then pure ()
          else recur (count + 1)

networkHandler :: ActorId -> TVar SeqNum -> TQueue (NetMessage msg) -> Handler msg
networkHandler sendAid snVar globalQueue appMsg = do
  sn <- allocSeqNum snVar
  let mid = MessageId sendAid sn
      netMsg = NetMessage mid appMsg
  writeTQueue globalQueue netMsg

actorBody :: ActorId -> TEvent -> TQueue (NetMessage msg) -> TQueue (LogEvent msg) -> Handler msg -> IO ()
actorBody aid doneEvent actorQueue logQueue handler = do
  processUntilDone doneEvent actorQueue $ \(NetMessage mid appMsg@(AppMessage recvAid _)) -> do
    if aid == recvAid
      then do
        handler appMsg
        writeTQueue logQueue (LogEventProcessed aid mid)
      else writeTQueue logQueue (LogEventMisdelivered aid mid aid)

-- | Run the actor system
runActorSystem :: Maybe TimeDelta -> ActorConstructor r msg -> [r] -> IO (TEvent, [ThreadId], MVar [LogEvent msg])
runActorSystem mayLimit ctor configs = do
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
  let timerSet = ctorResults >>= \(aid, tconfigs, _) -> fmap (aid,) tconfigs
  let actorSet = fmap (\(aid, _, behavior) -> (aid, behavior)) ctorResults
  -- Now that we know how many timers, create a barrier of that size
  timerBarrier <- newTBarrierIO (length timerSet)
  -- Spawn threads
  networkThreadId <- forkIO (networkBody doneEvent globalQueue logQueue actorQueues outVar)
  monitorThreadId <- forkIO (monitorBody doneEvent globalQueue actorQueues timerBarrier)
  timerThreadIds <- for timerSet $ \(aid, tconfig) ->
    let snVar = actorSnVars Map.! aid
    in forkIO (timerBody aid tconfig doneEvent snVar timerBarrier globalQueue)
  actorThreadIds <- for actorSet $ \(aid, behavior) ->
    let snVar = actorSnVars Map.! aid
        actorQueue = actorQueues Map.! aid
        handler = behavior (networkHandler aid snVar globalQueue)
    in forkIO (actorBody aid doneEvent actorQueue logQueue handler)
  let allThreadIds = [networkThreadId, monitorThreadId] ++ timerThreadIds ++ actorThreadIds
  pure (doneEvent, allThreadIds, outVar)
