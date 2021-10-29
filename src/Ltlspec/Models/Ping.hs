module Ltlspec.Models.Ping where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Ltlspec (Theory(..), SAS, scanSAS, Prop (..), propAlways, propForAllNested, propIf, Atom (..), propEventually, Binder (..))

pingResponsiveProp :: Prop
pingResponsiveProp =
  let isResponse = PropAnd (PropAtom (Atom "IsMesssage" ["y", "x", "n"])) (PropAtom (Atom "IsResponse" ["n", "m"]))
      eventuallyIsResponse = propEventually (PropExists (Binder "n" "MessageId") isResponse)
      body = propIf (PropAtom (Atom "IsMessage" ["x", "y", "m"])) eventuallyIsResponse
  in propAlways (propForAllNested [("x", "ActorId"), ("y", "ActorId"), ("m", "MessageId")] body)

pingTheory :: Theory
pingTheory = Theory
  { theoryTypes = ["ActorId", "MessageId"]
  , theoryProps = Map.fromList
      [ ("IsMessage", ["ActorId", "ActorId", "MessageId"])
      , ("IsResponse", ["MessageId", "MessageId"])
      ]
  , theoryAxioms = Map.fromList
      [ ("isResponsive", pingResponsiveProp)
      ]
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
updatePingState m s = case m of
  PingMessagePing from _ reqId -> Map.adjust (Set.insert reqId) from s
  PingMessagePong _ to reqId -> Map.adjust (Set.delete reqId) to s

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
