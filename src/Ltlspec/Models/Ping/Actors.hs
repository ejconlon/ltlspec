module Ltlspec.Models.Ping.Actors where

import Ltlspec.Models.Ping.Common (PingMessage (..))
import Ltlspec.System.Actors (ActorCase (ActorCase), ActorConstructor, ActorId, AnnoMessage, AppMessage (..), Behavior,
                              TickMessage (..), findActorsWhere, minimalTickMessageFilter, mkTickConfig,
                              runActorCaseSimple)
import Ltlspec.System.Logging (Logger, consoleLogger)
import Ltlspec.System.Time (TimeDelta, timeDeltaFromFracSecs)
import Text.Pretty.Simple (pPrint)

-- | In this example we have 3 roles. Each is going to ping the "next role".
data PingConfig =
    PingConfigRoleA
  | PingConfigRoleB
  deriving stock (Eq, Show, Enum, Bounded)

-- | Our roles are in a circle.
nextRole :: PingConfig -> PingConfig
nextRole = \case
  PingConfigRoleA -> PingConfigRoleB
  PingConfigRoleB -> PingConfigRoleA
  -- PingConfigRoleC -> PingConfigRoleA

-- | We'll instantiate one of each role.
pingConfigs :: [PingConfig]
pingConfigs = [PingConfigRoleA, PingConfigRoleB]

-- | Finds the actor id of the next role.
findNextRoleId :: PingConfig -> [(ActorId, PingConfig)] -> ActorId
findNextRoleId myRole = fst . head . findActorsWhere (== nextRole myRole)

-- | Defines the behavior for a ping actor.
-- On clock tick, send a ping to the actor playing the next role.
-- On recv ping message from anyone, send pong back.
pingBehavior :: ActorId -> Behavior (TickMessage PingMessage)
pingBehavior nextId sendMsg (AppMessage sendAid tm) =
  case tm of
    TickMessageFire -> sendMsg (AppMessage nextId (TickMessageEmbed PingMessagePing))
    TickMessageEmbed msg ->
      case msg of
        PingMessagePing -> sendMsg (AppMessage nextId (TickMessageEmbed PingMessagePong))
        _ -> pure ()

-- | Each actor is instantiated with a tick timer and the above behavior.
pingCtor :: Int -> TimeDelta -> ActorConstructor PingConfig (TickMessage PingMessage)
pingCtor limit interval pairs myId myRole =
  let otherId = findNextRoleId myRole pairs
      tickConfig = mkTickConfig Nothing interval (Just limit) myId
  in ([tickConfig], pingBehavior otherId)

-- | Wraps up our ctor and configs to define our ping system.
pingCase :: Int -> TimeDelta -> ActorCase PingMessage
pingCase limit interval = ActorCase (pingCtor limit interval) pingConfigs minimalTickMessageFilter

-- | Simulate the ping actor system and return a log of messages
runPingSim :: Logger -> Int -> TimeDelta -> IO [AnnoMessage PingMessage]
runPingSim logger limit interval = runActorCaseSimple logger (pingCase limit interval)

-- | Runs the actor system with a limit of 5 invocations of all tick timers.
-- Prints all non-tick-fire events at the end.
main :: IO ()
main = do
  logger <- consoleLogger
  messages <- runPingSim logger 5 (timeDeltaFromFracSecs (0.02 :: Double))
  pPrint messages
