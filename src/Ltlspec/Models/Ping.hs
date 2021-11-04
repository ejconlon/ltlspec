module Ltlspec.Models.Ping where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Ltlspec (Atom (..), Binder (..), Bridge (..), Error, Prop (..), SAS (..), Theory (..), propAlways,
                propEventually, propForAllNested, propIf, scanSAS)

-- | A proposition encoding responsiveness for ping messages.
-- Textually (but omitting types), this is equivalent to:
-- Always (Forall x y m. IsMessage x y m -> Eventually (Exists n. IsMessage y x n /\ IsResponse n m))
pingResponsiveProp :: Prop
pingResponsiveProp =
  let prop = propAlways (propForAllNested [("x", "ActorId"), ("y", "ActorId"), ("m", "MessageId")] ifMessageEventuallyResponse)
      ifMessageEventuallyResponse = propIf (PropAtom (Atom "IsMessage" ["x", "y", "m"])) eventuallyIsResponse
      eventuallyIsResponse = propEventually (PropExists (Binder "n" "MessageId") isResponse)
      isResponse = PropAnd (PropAtom (Atom "IsMessage" ["y", "x", "n"])) (PropAtom (Atom "IsResponse" ["n", "m"]))
  in prop

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
data MessageData = MessageData !ActorId !MessageId
  deriving stock (Eq, Ord, Show)
type PingData = Set MessageData
type PingState = Map ActorId PingData

emptyPingState :: PingState
emptyPingState = mempty

data PingMessage =
    PingMessagePing !ActorId !ActorId !MessageId
  | PingMessagePong !ActorId !ActorId !MessageId
  deriving stock (Eq, Show)

updatePingState :: PingMessage -> PingState -> PingState
updatePingState = \case
  PingMessagePing from to reqId -> Map.adjust (Set.insert (MessageData to reqId)) from
  PingMessagePong from to reqId -> Map.adjust (Set.delete (MessageData from reqId)) to

type PingWorld = SAS PingState PingMessage

-- | A trace of a sequence of messages that demonstrate responsiveness.
pingMessagesOk :: [PingMessage]
pingMessagesOk =
  [ PingMessagePing 0 1 42
  , PingMessagePing 1 0 78
  , PingMessagePong 1 0 79
  , PingMessagePong 0 1 43
  ]

-- | A trace of worlds corresponding to the sequence of messages.
pingWorldOk :: [PingWorld]
pingWorldOk = scanSAS updatePingState emptyPingState pingMessagesOk

data PingVal =
    PingValMessage !MessageId
  | PingValActor !ActorId
  deriving stock (Eq, Show)

-- TODO(ejconlon) Finish bridge and unit test it

evalIsMessage :: PingState -> ActorId -> ActorId -> MessageId -> Either Error Prop
evalIsMessage _ _ _ _ = Left "TODO"

evalIsResponse :: PingState -> MessageId -> MessageId -> Either Error Prop
evalIsResponse _ _ _ = Left "TODO"

instance Bridge Error PingVal PingWorld where
  bridgeEvalProp (SAS _ _ s) (Atom propName vals) =
    case (propName, vals) of
      ("IsMessage", [PingValActor x, PingValActor y, PingValMessage m]) -> evalIsMessage s x y m
      ("IsResponse", [PingValMessage n, PingValMessage m]) -> evalIsResponse s n m
      _ -> Left ("Could not eval " <> propName <> " on " <> show vals)
  bridgeQuantify w tyName = undefined
