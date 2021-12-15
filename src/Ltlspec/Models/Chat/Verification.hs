{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ltlspec.Models.Chat.Verification where

import Ltlspec.Types (Atom (..), Theory(..), SAS(..), Prop(..), Bridge(..), TruncBridge(..), Error, Commented (NoComment), SProp (..), BinderGroup (..), initScanSAS, ApplyAction(..))
import qualified Data.Map as Map
import Ltlspec.Models.Chat.Commons (ChatMessage(..), ChatState, ChannelID, ActionID(..))
import Ltlspec.System.Actors (ActorId(..), AnnoMessage (AnnoMessage), NetMessage (NetMessage), AppMessage(..))
import qualified Data.Set as Set
import Ltlspec.TriBool ( TriBool(..) )
chatTheory :: Theory
chatTheory = Theory
  { theoryTypes = NoComment <$> ["Client", "Channel", "Action"]
  , theoryProps = NoComment <$> Map.fromList
      [ ("IsMember", ["Client", "Channel"])
      , ("IsSameClient", ["Client", "Client"])
      , ("Left", ["Action", "Client", "Channel"])
      , ("Joined", ["Action", "Client", "Channel"])
      , ("ListRequested",["Action", "Client"])
      , ("Sent", ["Action", "Client", "Channel"])
      , ("Shared", ["Action", "Client", "Client"])
      , ("NewJoinNote", ["Action", "Client", "Channel", "Client"])
      , ("NewLeaveNote", ["Action", "Client", "Channel", "Client"])
      , ("ChannelListNote", ["Action", "Client", "Channel"])
      ]
  , theoryAxioms = NoComment <$> Map.fromList 
      [ ("isMemberBetweenJoinAndLeave",
          SPropAlways (
            SPropForAll [BinderGroup ["c"] "Client", BinderGroup ["ch"] "Channel"] (
              SPropExists [BinderGroup ["i"] "Action"] (
                SPropIf
                  [SPropAtom (Atom "Joined" ["i", "c", "ch"])]
                  (SPropExists [BinderGroup ["j"] "Action"] (
                    SPropUntil
                      (SPropAtom (Atom "IsMember" ["c","ch"]))
                      (SPropAtom (Atom "Left" ["j","c","ch"]))
                  ) 
                  )
              )
            )
          )
        )
        -- ("isMemberBetweenJoinAndLeave",
        --   SPropAlways (
        --     SPropForAll [BinderGroup ["c"] "Client", BinderGroup ["ch"] "Channel"] (
        --       SPropExists [BinderGroup ["i", "j"] "Action"] (
        --         SPropIf
        --           [SPropAtom (Atom "Joined" ["i", "c", "ch"])]
        --           (SPropUntil
        --             (SPropAtom (Atom "IsMember" ["c","ch"]))
        --             (SPropAtom (Atom "Left" ["j","c","ch"]))
        --           )
        --       )
        --     )
        --   )
        -- )
      , ("ifInChannelReceiveMessage",
          SPropAlways (
            SPropForAll [BinderGroup ["c1", "c2"] "Client", BinderGroup ["ch"] "Channel", BinderGroup ["m"] "Action"] (
              SPropIf
                [ SPropAnd
                  [ SPropNot (SPropAtom (Atom "IsSameClient" ["c1", "c2"]))
                  , SPropAtom (Atom "IsMember" ["c1", "ch"])
                  , SPropAtom (Atom "IsMember" ["c2", "ch"])
                  , SPropAtom (Atom "Sent" ["m", "c1", "ch"])
                  ]
                ]
                (SPropEventually (SPropAtom (Atom "Shared" ["m", "c1", "c2"])))
            )
          )
        )
      , 
      ("neverSendMessageToMyself",
          SPropAlways (
            SPropForAll [BinderGroup ["c"] "Client", BinderGroup ["m"] "Action"] (
              SPropNot (SPropAtom (Atom "Shared" ["m", "c", "c"]))
            )
          )
        )
      ]
  }
    

newtype ChatWorld = ChatWorld {unChatWorld :: SAS ChatState (AnnoMessage ChatMessage)}

data ChatVal =
    ChatValClient ActorId
  | ChatValChannel ChannelID
  | ChatValAction ActionID
  deriving stock (Eq, Show)

evalChatProp :: ChatWorld -> Atom ChatVal -> Either Error Prop
evalChatProp (ChatWorld (SAS _ m s2) ) (Atom propName vals) = let AnnoMessage _ (NetMessage _ (AppMessage _ e)) = m in
        case (propName, vals) of
            ("IsMember", [ChatValClient cid, ChatValChannel chid]) -> if chid `elem` Map.findWithDefault [] cid (fst s2) then Right PropTrue else Right PropFalse
            ("IsSameClient", [ChatValClient cid1, ChatValClient cid2]) -> if cid1 == cid2 then Right PropTrue else Right PropFalse
            ("Left", [ChatValAction aid, ChatValClient cid, ChatValChannel chid]) -> if e == Leave aid cid chid then Right PropTrue else Right PropFalse
            ("Joined", [ChatValAction aid, ChatValClient cid, ChatValChannel chid]) -> if e == Join aid cid chid then Right PropTrue else Right PropFalse
            ("Sent", [ChatValAction aid, ChatValClient cid, ChatValChannel chid]) -> case e of
                                                                                        Send a c _ ch -> if a==aid && c==cid && ch==chid then Right PropTrue  else Right PropFalse
                                                                                        _ -> Right PropFalse
            ("ListRequested", [ChatValAction aid, ChatValClient cid]) -> if e == List aid cid then Right PropTrue else Right PropFalse
            ("Shared", [ChatValAction aid, ChatValClient sid, ChatValClient rid]) -> case e of
                                                                                        Share a s _ r -> if a==aid && s==sid && r==rid then Right PropTrue else Right PropFalse
                                                                                        _ -> Right PropFalse
            ("NewJoinNote", [ChatValAction aid, ChatValClient cid1, ChatValChannel chid, ChatValClient cid2]) -> if e == NewJoin aid cid1 chid cid2 then Right PropTrue else Right PropFalse
            ("NewLeaveNote", [ChatValAction aid, ChatValClient cid1, ChatValChannel chid, ChatValClient cid2]) -> if e == NewLeave aid cid1 chid cid2 then Right PropTrue else Right PropFalse
            ("ChannelListNote", [ChatValAction aid, ChatValClient cid, ChatValChannel chid]) -> if e == ChannelList aid cid chid then Right PropTrue else Right PropFalse
            _ -> Left ("Could not eval " <> propName <> " on " <> show vals)

processEvent :: ChatState -> ChatMessage -> ChatState
processEvent (cState, seed) event =
    case event of
        NoOp -> (cState, seed)
        (List aid _) -> (cState, unActionID aid)
        (Join aid cid chid) -> (Map.insert cid  (filter (/= chid) (Map.findWithDefault [] cid cState) ++ [chid] ) cState, unActionID aid)
        (Leave aid cid chid) -> (Map.insert cid  (filter (/= chid) (Map.findWithDefault [] cid cState)) cState, unActionID aid)
        (Send aid _ _ _) -> (cState, unActionID aid)
        (Share aid _ _ _) -> (cState, unActionID aid)
        (NewJoin aid _ _ _) -> (cState, unActionID aid)
        (NewLeave aid _ _ _) -> (cState, unActionID aid)
        (ChannelList aid _ _) -> (cState, unActionID aid)
        StartService -> (cState, seed)


initialChatState :: Int -> ChatState
initialChatState nclients = (Map.fromList [(ActorId i,[]) | i <- [1..nclients]], 0)

-- generateTraceGivenMessages :: Int -> [ChatMessage] -> [ChatWorld]
-- generateTraceGivenMessages nclients = initScanSAS processEvent (initialChatState nclients)

instance ApplyAction (AnnoMessage ChatMessage) ChatState where
  applyAction w msg = 
    case msg of 
      AnnoMessage _ (NetMessage _ (AppMessage _ m)) -> processEvent w m

instance Bridge Error ChatVal ChatWorld where
    bridgeEvalProp = evalChatProp

    -- TODO quantify actions with the action component of the SAS
    bridgeQuantify (ChatWorld (SAS _ _ s2)) tyname =
        case tyname of
            "Client" -> Right (map ChatValClient (Map.keys (fst s2)))
            "Channel" -> Right (map ChatValChannel (Set.toList (Set.fromList (concat (Map.elems (fst s2))))))
            "Action" -> Right (map (ChatValAction . ActionID) [0..(snd s2)] )
            _ -> Left ("Could not quantify over " <> tyname)

instance TruncBridge Error ChatVal ChatWorld where
    truncBridgeEmpty _ = Set.fromList ["Client", "Channel", "Action"]
    truncBridgeOracle w p =
      let prop = evalChatProp w p in
      case prop of
          Left e -> Left e
          Right q ->
            case q of
              PropTrue -> Right TriBoolTrue
              PropFalse -> Right TriBoolFalse
              _ -> Right TriBoolUnknown