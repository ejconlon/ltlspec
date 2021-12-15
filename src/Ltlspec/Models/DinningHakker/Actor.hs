module Ltlspec.Models.DinningHakker.Actor where
import Control.Concurrent.STM (TVar, newTVarIO, readTVar, writeTVar)
import Control.Monad (replicateM)
import Data.Maybe (fromJust)
import Ltlspec.System.Actors (ActorCase (..), ActorConstructor, ActorId, AnnoMessage, AppMessage (AppMessage), Behavior,
                              TickMessage (..), findActorsWhere, minimalTickMessageFilter, mkTickConfig,
                              runActorCaseSimple)
import Ltlspec.System.Logging (Logger, consoleLogger)
import Ltlspec.System.Time (TimeDelta, timeDeltaFromFracSecs)
import Text.Pretty.Simple (pPrint)

type DHId = Int

-- | chopstick state
data DHChopstickState =
    Taken
  | Free
  deriving stock (Eq, Show)

-- | hakker state
data DHHakkerState =
    Thinking
  -- The Int stands for the number of chopsticks the hakker currently holding
  | Hungry Int
  | Eating
  deriving stock (Eq, Show)

-- | actor config in dinning hakker
data DHConfig =
    DHHakker DHId DHId (TVar DHHakkerState)
  | DHChopstick DHId (TVar DHChopstickState)
  deriving stock (Eq)

data DHMessage =
    DHTake -- Hakker attemps to take chopstick
  | DHPut -- Hakker attempts to put down chopstick
  | DHTaken -- Chopstick is taken by the receving hakker
  | DHBusy -- Chopstick is busy being taken by another hakker
  deriving stock (Eq, Show)

dhGenConfigs :: Int -> [TVar DHHakkerState] -> [TVar DHChopstickState] -> [DHConfig]
dhGenConfigs n hs cs =
  [DHHakker ((i-1+n) `mod` n) i (hs !! i) | i <- [0..n-1]] ++
  [DHChopstick i (cs !! i) | i <- [1..n]]

dhConfigs :: IO [DHConfig]
dhConfigs = do
  hstates <- replicateM 10 (newTVarIO Thinking)
  cstates <- replicateM 10 (newTVarIO Free)
  return $ dhGenConfigs 10 hstates cstates

findChopsticks :: DHConfig -> [(ActorId, DHConfig)] -> (ActorId, ActorId)
findChopsticks (DHHakker l r _) as = case findActorsWhere findLR as of
  [(lid, _), (rid, _)] -> (lid, rid)
  _ -> error "Found more than two adjacent "
  where
    findLR = \case
      DHHakker {} -> False
      DHChopstick i _ -> l == i || r == i
findChopsticks DHChopstick {} _ = error "Should not use findChopsticks on chopstick actors"

data DHArgs =
  DHArgs ActorId DHConfig (Maybe (ActorId, ActorId))

-- This is an ugly design:
-- The third argument for Hakker stands for its left and right chopstick actor ids
-- The third argument for Chopstick it should always be Nothing
dhBehavior :: ActorId -> DHConfig -> Maybe (ActorId, ActorId) -> Behavior (TickMessage DHMessage)
dhBehavior _ (DHHakker _ _ stvar) lr sendMsg (AppMessage sendAid tm) = case tm of
  TickMessageFire ->
    let (lid, rid) = fromJust lr in do
    st <- readTVar stvar
    case st of
      Thinking -> do
        sendMsg (AppMessage lid (TickMessageEmbed DHTake))
        sendMsg (AppMessage rid (TickMessageEmbed DHTake))
        writeTVar stvar (Hungry 0)
      Eating -> do
        sendMsg (AppMessage lid (TickMessageEmbed DHPut))
        sendMsg (AppMessage rid (TickMessageEmbed DHPut))
        writeTVar stvar Thinking
      _ -> return ()
  TickMessageEmbed msg -> case msg of
    DHTaken -> do
      st <- readTVar stvar
      case st of
        Hungry n
          | n == 0 -> writeTVar stvar (Hungry 1)
          | n == 1 -> writeTVar stvar Eating
          | otherwise -> error "Hakker cannot hold more than 2 chopsticks"
        _ -> error "Wrong state for hakker to receive DHTaken message"
    DHBusy -> sendMsg (AppMessage sendAid (TickMessageEmbed DHTake))
    _ -> error ("Wrong type of message received by Hakker: " ++ show msg)
dhBehavior aid (DHChopstick _ stvar) _ sendMsg (AppMessage sendAid tm) = case tm of
  TickMessageFire -> return ()
  TickMessageEmbed msg -> case msg of
    DHTake -> do
      st <- readTVar stvar
      case st of
        Taken -> sendMsg (AppMessage sendAid (TickMessageEmbed DHBusy))
        Free -> do
          sendMsg (AppMessage sendAid (TickMessageEmbed DHTaken))
          writeTVar stvar Taken
    DHPut -> do
      st <- readTVar stvar
      case st of
        Taken -> do
          writeTVar stvar Free
        Free -> error "Chopstick should not receive Put message when Free"
    _ -> error ("Wrong type of message received by Chopstick " ++ show aid ++ ": " ++ show msg)

dhCtor :: Int -> TimeDelta -> ActorConstructor DHConfig (TickMessage DHMessage)
dhCtor limit interval pairs myId = \case
  hk@DHHakker{} ->
    let chops = findChopsticks hk pairs
        tickConfig = mkTickConfig Nothing interval (Just limit) myId
    in ([tickConfig], dhBehavior myId hk (Just chops))
  chop@DHChopstick{} -> ([], dhBehavior myId chop Nothing)

dhCase :: Int -> TimeDelta -> [DHConfig] -> ActorCase DHMessage
dhCase limit interval pairs = ActorCase (dhCtor limit interval) pairs minimalTickMessageFilter

runDhSim :: Logger -> Int -> TimeDelta -> [DHConfig] -> IO [AnnoMessage DHMessage]
runDhSim logger limit interval pairs = runActorCaseSimple logger (dhCase limit interval pairs)

main :: IO ()
main = do
  logger <- consoleLogger
  configs <- dhConfigs
  messages <- runDhSim logger 10 (timeDeltaFromFracSecs (1 :: Double)) configs
  pPrint messages
