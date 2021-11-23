module Ltlspec.System.Example
  ( main
  ) where

import Ltlspec.System.Actors (ActorConstructor, ActorId, AppMessage (..), Behavior, TickMessage (..),
                              filterRecvMessages, filterTickEvents, findActorsWhere, mkTickConfig, runActorSystemSimple)
import Ltlspec.System.Time (TimeDelta, timeDeltaFromFracSecs)
import Text.Pretty.Simple (pPrint)

-- | In this example we have 3 roles. Each is going to ping the "next role".
data PingConfig =
    PingConfigRoleA
  | PingConfigRoleB
  | PingConfigRoleC
  deriving stock (Eq, Show, Enum, Bounded)

-- | We're sending pings and receiving pongs.
data PingMessage =
    PingMessagePing
  | PingMessagePong
  deriving stock (Eq, Show)

-- | Our roles are in a circle.
nextRole :: PingConfig -> PingConfig
nextRole = \case
  PingConfigRoleA -> PingConfigRoleB
  PingConfigRoleB -> PingConfigRoleC
  PingConfigRoleC -> PingConfigRoleA

-- | We'll instantiate one of each role.
pingConfigs :: [PingConfig]
pingConfigs = [PingConfigRoleA, PingConfigRoleB, PingConfigRoleC]

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
        PingMessagePing -> sendMsg (AppMessage sendAid (TickMessageEmbed PingMessagePong))
        _ -> pure ()

-- | Each actor is instantiated with a tick timer and the above behavior.
pingCtor :: Int -> TimeDelta -> ActorConstructor PingConfig (TickMessage PingMessage)
pingCtor limit interval pairs myId myRole =
  let otherId = findNextRoleId myRole pairs
      tickConfig = mkTickConfig Nothing interval (Just limit) myId
  in ([tickConfig], pingBehavior otherId)

-- | Runs the actor system with a limit of 5 invocations of all tick timers.
-- Prints all non-tick-fire events at the end.
main :: IO ()
main = do
  let limit = 5
      interval = timeDeltaFromFracSecs (0.02 :: Double)
      ctor = pingCtor limit interval
  logEvents <- runActorSystemSimple ctor pingConfigs
  let messages = filterRecvMessages (filterTickEvents logEvents)
  pPrint messages
