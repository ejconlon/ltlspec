module Ltlspec.Models.Ping where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Ltlspec (Theory(..), SAS, scanSAS, Prop (..), propAlways, propForAllNested, propIf, Atom (..), propEventually, Binder (..))

-- | A proposition encoding responsiveness for ping messages.
-- Textually (but omitting types), this is equivalent to:
-- Always (Forall x y m. IsMessage x y m -> Eventually (Exists n. IsMessage y x n /\ IsResponse n m))
pingResponsiveProp :: Prop
pingResponsiveProp =
  let prop = propAlways (propForAllNested [("x", "ActorId"), ("y", "ActorId"), ("m", "MessageId")] ifMessageEventuallyResponse)
      ifMessageEventuallyResponse = propIf (PropAtom (Atom "IsMessage" ["x", "y", "m"])) eventuallyIsResponse
      eventuallyIsResponse = propEventually (PropExists (Binder "n" "MessageId") isResponse)
      isResponse = PropAnd (PropAtom (Atom "IsMesssage" ["y", "x", "n"])) (PropAtom (Atom "IsResponse" ["n", "m"]))
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
type PingData = Set MessageId
type PingState = Map ActorId PingData

emptyPingState :: PingState
emptyPingState = mempty

data PingMessage =
    PingMessagePing !ActorId !ActorId !MessageId
  | PingMessagePong !ActorId !ActorId !MessageId
  deriving stock (Eq, Show)

updatePingState :: PingMessage -> PingState -> PingState
updatePingState = \case
  PingMessagePing from _ reqId -> Map.adjust (Set.insert reqId) from
  PingMessagePong _ to reqId -> Map.adjust (Set.delete reqId) to

type PingWorld = SAS PingState PingMessage

-- | A trace of a sequence of messages that demonstrate responsiveness.
pingMessagesOk :: [PingMessage]
pingMessagesOk =
  [ PingMessagePing 0 1 42
  , PingMessagePing 1 0 43
  , PingMessagePong 1 0 42
  , PingMessagePong 0 1 43
  ]

-- | A trace of worlds corresponding to the sequence of messages.
pingWorldOk :: [PingWorld]
pingWorldOk = scanSAS updatePingState emptyPingState pingMessagesOk
