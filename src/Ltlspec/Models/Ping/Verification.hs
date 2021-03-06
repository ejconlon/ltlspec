module Ltlspec.Models.Ping.Verification where

import Control.DeepSeq (NFData)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Ltlspec.Models.Ping.Common (PingAction, PingMessage (..))
import Ltlspec.System.Actors (ActorId, AnnoMessage (..), AppMessage (..), MessageId (..), MessageView (..),
                              NetMessage (..))
import Ltlspec.TriBool (TriBool (..))
import Ltlspec.Types (ApplyAction (..), Atom (..), BinderGroup (..), Bridge (..), Commented (..), Error, Prop (..),
                      SAS (..), SProp (..), Theory (..), TruncBridge (..), initScanSAS)

-- | A proposition encoding responsiveness for ping messages.
-- Textually, this is equivalent to:
-- Always (Forall (m1: SentPing). Eventually (Exists (m2: RecvPong). IsPingPong m1 m2))
pingResponsiveProp :: SProp
pingResponsiveProp =
  let prop = SPropAlways (SPropForAll [BinderGroup ["m1"] "SentPing"] eventuallyPong)
      eventuallyPong = SPropEventually (SPropExists [BinderGroup ["m2"] "RecvPong"] pong)
      pong = SPropAtom (Atom "IsPingPong" ["m1", "m2"])
  in prop

pingTheory :: Theory
pingTheory = Theory
  { theoryTypes =
      [ YesComment "SentPing" "A ping request sent by an actor"
      , YesComment "RecvPong" "A pong response received by an actor"
      ]
  , theoryProps = Map.fromList
      [ ("IsPingPong", YesComment ["SentPing", "RecvPong"] "Characterizes a request-response pair")
      ]
  , theoryAxioms = Map.fromList
      [ ("isResponsive", YesComment pingResponsiveProp "Every ping request eventually gets a pong response")
      ]
  }

newtype PingState = PingState { unPingState :: Map ActorId (Set ActorId) }
  deriving stock (Show)
  deriving newtype (Eq, NFData)

emptyPingState :: PingState
emptyPingState = PingState Map.empty

instance ApplyAction PingAction PingState where
  applyAction ps@(PingState st) (AnnoMessage view (NetMessage (MessageId sendAid _) (AppMessage recvAid pay))) =
    case (view, pay) of
      (MessageViewSent, PingMessagePing) -> PingState (Map.adjust (Set.insert recvAid) sendAid st)
      (MessageViewReceived, PingMessagePong) -> PingState (Map.adjust (Set.delete sendAid) recvAid st)
      _ -> ps

-- Note: This is a newtype and not a type synonym to avoid an orphan Bridge instance.
newtype PingWorld = PingWorld { unPingWorld :: SAS PingState PingAction }
  deriving stock (Eq)
  deriving newtype (Show, NFData, ApplyAction PingAction)

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

-- | A trace of a sequence of messages that demonstrate responsiveness.
pingMessagesNotOk :: [PingAction]
pingMessagesNotOk =
  let ping01 = NetMessage (MessageId 0 42) (AppMessage 1 PingMessagePing)
      ping10 = NetMessage (MessageId 1 78) (AppMessage 0 PingMessagePing)
      pong01 = NetMessage (MessageId 1 79) (AppMessage 0 PingMessagePong)
  in [ AnnoMessage MessageViewSent ping01
     , AnnoMessage MessageViewSent ping10
     , AnnoMessage MessageViewReceived ping01
     , AnnoMessage MessageViewReceived ping10
     , AnnoMessage MessageViewSent pong01
     , AnnoMessage MessageViewReceived pong01
     ]

-- | Make a sequence of worlds from a sequence of actions
mkPingWorlds :: [PingAction] -> [PingWorld]
mkPingWorlds = fmap PingWorld . initScanSAS applyAction emptyPingState

-- | A trace of worlds corresponding to the ok sequence of messages.
pingWorldsOk :: [PingWorld]
pingWorldsOk = mkPingWorlds pingMessagesOk

-- | A trace of worlds corresponding to the not ok sequence of messages.
pingWorldsNotOk :: [PingWorld]
pingWorldsNotOk = mkPingWorlds pingMessagesNotOk

evalIsPingPong :: PingAction -> PingAction -> Bool
evalIsPingPong am1 am2 =
  let AnnoMessage view1 (NetMessage (MessageId sendAid1 _) (AppMessage recvAid1 msg1)) = am1
      AnnoMessage view2 (NetMessage (MessageId sendAid2 _) (AppMessage recvAid2 msg2)) = am2
  in case (view1, msg1, view2, msg2) of
    (MessageViewSent, PingMessagePing, MessageViewReceived, PingMessagePong) | sendAid1 == recvAid2 && recvAid1 == sendAid2 -> True
    _ -> False

evalPingProp :: Atom PingAction -> Either Error Bool
evalPingProp (Atom propName vals) =
  case (propName, vals) of
    ("IsPingPong", [m1, m2]) -> Right (evalIsPingPong m1 m2)
    _ -> Left ("Could not eval " <> propName <> " on " <> show vals)

instance Bridge Error PingAction PingWorld where
  bridgeEvalProp _ = fmap (\b -> if b then PropTrue else PropFalse) . evalPingProp
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

instance TruncBridge Error PingAction PingWorld where
  truncBridgeEmpty _ = Set.fromList ["SentPing", "RecvPong"]
  truncBridgeOracle _ = fmap (\b -> if b then TriBoolTrue else TriBoolFalse) . evalPingProp
