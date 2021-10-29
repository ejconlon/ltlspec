module Ltlspec.Models.Ping where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
-- import qualified Data.Set as Set
import Ltlspec (Theory(..), SAS, scanSAS)

pingTheory :: Theory
pingTheory = Theory
  { theoryTypes = ["ActorId", "MessageId"]
  , theoryProps = Map.fromList
      [ ("IsMessage", ["ActorId", "MessageId"])
      , ("IsResponse", ["MessageId", "MessageId"])
      ]
  , theoryAxioms = mempty -- Map.fromList []
  }

type ActorId = Int
type MessageId = Int
type PingData = Set MessageId
type PingState = Map ActorId PingData

emptyPingState :: PingState
emptyPingState = mempty

data PingMessage =
    PingMessagePing !ActorId !ActorId !MessageId
  | PingMessagePong !ActorId !ActorId !MessageId
  deriving stock (Eq, Show)

updatePingState :: PingMessage -> PingState -> PingState
updatePingState = undefined

type PingWorld = SAS PingState PingMessage

pingMessagesOk :: [PingMessage]
pingMessagesOk =
  [ PingMessagePing 0 1 42
  , PingMessagePing 1 0 43
  , PingMessagePong 1 0 42
  , PingMessagePong 0 1 43
  ]

pingWorldOk :: [PingWorld]
pingWorldOk = scanSAS updatePingState emptyPingState pingMessagesOk
