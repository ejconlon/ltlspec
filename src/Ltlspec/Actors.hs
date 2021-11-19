module Ltlspec.Actors where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM (STM, newTQueue, newTQueueIO, TQueue, flushTQueue, writeTQueue)
import Data.Traversable (for)
import Data.Foldable (for_)
import Control.Monad.STM (atomically)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type ActorId = Int

data Message msg = Message
  { messageSender :: !ActorId
  , messageReceiver :: !ActorId
  , messagePayload :: !msg
  } deriving stock (Eq, Show)

type Handler msg = ActorId -> msg -> STM ()

type Behavior msg = Handler msg -> Handler msg

type ActorConstructor r msg = Map ActorId r -> ActorId -> ([Timer msg], Behavior msg)

data ChatRole = ChatRoleServer | ChatRoleClient !String
  deriving stock (Eq, Show)

-- These delays are in us (microseconds), as threadDelay is
data Timer msg = Timer
  { timerStartDelay :: !Int
  -- ^ Amount of time to delay first invocation
  , timerPeriodicInterval :: !(Maybe Int)
  -- ^ If this is Just then we repeatedly invoke the timer
  , timerMessages :: ![Message msg]
  -- ^ Message to send on every invocation - TODO make sure sender is always this actor!
  } deriving stock (Eq, Show)

actorThread :: TQueue (Message msg) -> Handler msg -> IO ()
actorThread actorQueue body = error "TODO" -- pull from actor queue and handle

networkFor :: TQueue (Message msg) -> ActorId -> Handler msg
networkFor globalQueue sendAid recvAid msg = writeTQueue globalQueue (Message sendAid recvAid msg)

-- time limit, handler, number of actors,
runActors :: Int -> ActorConstructor r msg -> [r] -> IO [Message msg]
runActors timeLimit ctor configs = do
  globalQueue <- newTQueueIO
  globalLog <- newTQueueIO
  -- TODO create event var for graceful termination
  -- TODO fork the network handler
  -- TODO create actorQueues (Map ActorId (TQueue (Message msg)))
  let configMap = Map.fromList (zip [0..] configs)
  threadIds <- for (Map.keys configMap) $ \aid ->
    let network = networkFor globalQueue aid
        (timers, body) = ctor configMap aid
        -- TODO fork the timers
        action = actorThread (error "TODO take actorQueue") (body network)
    in forkIO action
  -- TODO use event var for termination instead of thread delay and kill
  threadDelay timeLimit
  for_ threadIds killThread
  atomically (flushTQueue globalLog)
