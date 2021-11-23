module Ltlspec.Models.Ping.Verification where

import Control.DeepSeq (NFData)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Ltlspec (propAlways, propEventually, propForAllNested, propIf, scanSAS)
import Ltlspec.Models.Ping.Common (PingMessage (..))
import Ltlspec.System.Actors (ActorId, AppMessage (..), MessageId (..), NetMessage (..))
import Ltlspec.Types (Atom (..), Binder (..), Bridge (..), Error, Prop (..), SAS (..), Theory (..))

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

type PingData = Set MessageId
type PingState = Map ActorId PingData

emptyPingState :: PingState
emptyPingState = mempty

type PingAction = NetMessage PingMessage

updatePingState :: PingAction -> PingState -> PingState
updatePingState = \case
  NetMessage mid (AppMessage recvAid pay) ->
    case pay of
      PingMessagePing -> Map.adjust (Set.insert mid) recvAid
      PingMessagePong -> Map.adjust (Set.delete mid) recvAid

newtype PingWorld = PingWorld { unPingWorld :: SAS PingState PingAction }
  deriving stock (Eq)
  deriving newtype (Show, NFData)

-- | A trace of a sequence of messages that demonstrate responsiveness.
pingMessagesOk :: [PingAction]
pingMessagesOk =
  [ NetMessage (MessageId 0 42) (AppMessage 1 PingMessagePing)
  , NetMessage (MessageId 1 78) (AppMessage 0 PingMessagePing)
  , NetMessage (MessageId 1 79) (AppMessage 0 PingMessagePong)
  , NetMessage (MessageId 0 43) (AppMessage 1 PingMessagePong)
  ]

-- | A trace of worlds corresponding to the sequence of messages.
pingWorldOk :: [PingWorld]
pingWorldOk = fmap PingWorld (scanSAS updatePingState emptyPingState pingMessagesOk)

data PingVal =
    PingValMessage !MessageId
  | PingValActor !ActorId
  deriving stock (Eq, Show)

-- TODO(ejconlon) Finish bridge and unit test it

evalIsMessage :: PingState -> ActorId -> ActorId -> MessageId -> Either Error Prop
evalIsMessage _ _ _ _ = error "TODO"

evalIsResponse :: PingState -> MessageId -> MessageId -> Either Error Prop
evalIsResponse _ _ _ = error "TODO"

instance Bridge Error PingVal PingWorld where
  bridgeEvalProp (PingWorld (SAS _ _ s)) (Atom propName vals) =
    case (propName, vals) of
      ("IsMessage", [PingValActor x, PingValActor y, PingValMessage m]) -> evalIsMessage s x y m
      ("IsResponse", [PingValMessage n, PingValMessage m]) -> evalIsResponse s n m
      _ -> Left ("Could not eval " <> propName <> " on " <> show vals)
  bridgeQuantify _ _ = error "TODO"
