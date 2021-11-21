module Ltlspec.System.Example
  ( main
  ) where

import Ltlspec.System.Actors (ActorConstructor, ActorId, AppMessage (..), Behavior, TickMessage (..), filterTickEvents,
                              findActorsWhere, mkTickConfig, runActorSystemSimple)
import Ltlspec.System.Time (TimeDelta, timeDeltaFromFracSecs)
import Text.Pretty.Simple (pPrint)

data PingConfig =
    PingConfigRoleA
  | PingConfigRoleB
  | PingConfigRoleC
  deriving stock (Eq, Show, Enum, Bounded)

data PingMessage =
    PingMessagePing
  | PingMessagePong
  deriving stock (Eq, Show)

nextRole :: PingConfig -> PingConfig
nextRole = \case
  PingConfigRoleA -> PingConfigRoleB
  PingConfigRoleB -> PingConfigRoleC
  PingConfigRoleC -> PingConfigRoleA

pingConfigs :: [PingConfig]
pingConfigs = [PingConfigRoleA, PingConfigRoleB, PingConfigRoleC]

findNextRoleId :: PingConfig -> [(ActorId, PingConfig)] -> ActorId
findNextRoleId myRole = fst . head . findActorsWhere (== nextRole myRole)

pingBehavior :: ActorId -> Behavior (TickMessage PingMessage)
pingBehavior nextId sendMsg (AppMessage sendAid tm) =
  case tm of
    TickMessageFire -> sendMsg (AppMessage nextId (TickMessageEmbed PingMessagePing))
    TickMessageEmbed msg ->
      case msg of
        PingMessagePing -> sendMsg (AppMessage sendAid (TickMessageEmbed PingMessagePong))
        _ -> pure ()

pingCtor :: Int -> TimeDelta -> ActorConstructor PingConfig (TickMessage PingMessage)
pingCtor limit interval pairs myId myRole =
  let otherId = findNextRoleId myRole pairs
      tickConfig = mkTickConfig Nothing interval (Just limit) myId
  in ([tickConfig], pingBehavior otherId)

main :: IO ()
main = do
  let limit = 5
      interval = timeDeltaFromFracSecs (0.1 :: Double)
      ctor = pingCtor limit interval
  logEvents <- runActorSystemSimple ctor pingConfigs
  let nonTickEvents = filterTickEvents logEvents
  pPrint nonTickEvents
