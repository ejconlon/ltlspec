module Ltlspec.Models.Ping.Verification where

import Control.DeepSeq (NFData)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Ltlspec (propAlways, propEventually, propExistsNested, propForAllNested, scanSAS)
import Ltlspec.Models.Ping.Common (PingMessage (..))
import Ltlspec.System.Actors (ActorId, AnnoMessage (..), AppMessage (..), MessageId (..), MessageView (..),
                              NetMessage (..))
import Ltlspec.Types (Atom (..), Bridge (..), Error, Prop (..), SAS (..), Theory (..))

-- | A proposition encoding responsiveness for ping messages.
-- Textually (but omitting types), this is equivalent to:
-- Always (Forall a1 m1 a2. IsPing a1 m1 a2 -> Eventually (Exists m2. IsPong a2 m2 a1))
pingResponsiveProp :: Prop
pingResponsiveProp =
  let prop = propAlways (propForAllNested [("m1", "SentPing")] eventuallyPong)
      eventuallyPong = propEventually (propExistsNested [("m2", "RecvPong")] pong)
      pong = PropAtom (Atom "PingPong" ["m1", "m2"])
  in prop

pingTheory :: Theory
pingTheory = Theory
  { theoryTypes = ["SentPing", "RecvPong"]
  , theoryProps = Map.fromList
      [ ("PingPong", ["SentPing", "RecvPong"])
      ]
  , theoryAxioms = Map.fromList
      [ ("isResponsive", pingResponsiveProp)
      ]
  }

type PingData = Set ActorId
type PingState = Map ActorId PingData

emptyPingState :: PingState
emptyPingState = mempty

type PingAction = AnnoMessage PingMessage

updatePingState :: PingAction -> PingState -> PingState
updatePingState (AnnoMessage view (NetMessage (MessageId sendAid _) (AppMessage recvAid pay))) =
  case (view, pay) of
    (MessageViewSent, PingMessagePing) -> Map.adjust (Set.insert recvAid) sendAid
    (MessageViewReceived, PingMessagePong) -> Map.adjust (Set.delete sendAid) recvAid
    _ -> id

-- Note: This is a newtype and not a type synonym to avoid an orphan Bridge instance.
newtype PingWorld = PingWorld { unPingWorld :: SAS PingState PingAction }
  deriving stock (Eq)
  deriving newtype (Show, NFData)

-- | A trace of a sequence of messages that demonstrate responsiveness.
pingMessagesOk :: [PingAction]
pingMessagesOk =
  let ping01 = NetMessage (MessageId 0 42) (AppMessage 1 PingMessagePing)
      ping10 = NetMessage (MessageId 1 78) (AppMessage 0 PingMessagePing)
      pong01 = NetMessage (MessageId 1 79) (AppMessage 0 PingMessagePong)
      pong10 = NetMessage (MessageId 0 43) (AppMessage 1 PingMessagePong)
  in [ AnnoMessage MessageViewSent ping01
     , AnnoMessage MessageViewSent ping10
     , AnnoMessage MessageViewReceived ping01
     , AnnoMessage MessageViewReceived ping10
     , AnnoMessage MessageViewSent pong10
     , AnnoMessage MessageViewReceived pong10
     , AnnoMessage MessageViewSent pong01
     , AnnoMessage MessageViewReceived pong01
     ]

-- | A trace of worlds corresponding to the sequence of messages.
pingWorldOk :: [PingWorld]
pingWorldOk = fmap PingWorld (scanSAS updatePingState emptyPingState pingMessagesOk)

evalPingPong :: PingAction -> PingAction -> Prop
evalPingPong am1 am2 =
  let AnnoMessage view1 (NetMessage (MessageId sendAid1 _) (AppMessage recvAid1 msg1)) = am1
      AnnoMessage view2 (NetMessage (MessageId sendAid2 _) (AppMessage recvAid2 msg2)) = am2
  in case (view1, msg1, view2, msg2) of
    (MessageViewSent, PingMessagePing, MessageViewReceived, PingMessagePong) | sendAid1 == recvAid2 && recvAid1 == sendAid2 -> PropTrue
    _ -> PropFalse

instance Bridge Error PingAction PingWorld where
  bridgeEvalProp (PingWorld _) (Atom propName vals) =
    case (propName, vals) of
      ("PingPong", [m1, m2]) -> Right (evalPingPong m1 m2)
      _ -> Left ("Could not eval " <> propName <> " on " <> show vals)
  bridgeQuantify (PingWorld (SAS _ a _)) tyName =
    case tyName of
      "SentPing" ->
        case a of
          AnnoMessage MessageViewSent (NetMessage _ (AppMessage _ PingMessagePing)) -> Right [a]
          _ -> Right []
      "RecvPong" ->
        case a of
          AnnoMessage MessageViewReceived (NetMessage _ (AppMessage _ PingMessagePong)) -> Right [a]
          _ -> Right []
      _ -> Left ("Could not quantify type " <> tyName)
