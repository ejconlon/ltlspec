{-# LANGUAGE OverloadedStrings #-}

module Ltlspec.Models.Chat.Commons where
import Ltlspec.System.Actors (ActorId)
import qualified Data.Map as Map
import Ltlspec.Types (SAS)
import Data.Hashable (Hashable)
import Control.DeepSeq (NFData)


newtype ActionID = ActionID {unActionID::Int}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Hashable, NFData)

newtype ChannelID = ChannelID {unChannelID::Int}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Hashable, NFData)

type MessageContent = String

data ChatMessage =
  NoOp
  | List ActionID ActorId
  | Join ActionID ActorId ChannelID
  | Leave ActionID ActorId ChannelID
  | Send ActionID ActorId MessageContent ChannelID
  | Share ActionID ActorId MessageContent ActorId
  | NewJoin ActionID ActorId ChannelID ActorId
  | NewLeave ActionID ActorId ChannelID ActorId
  | ChannelList ActionID ActorId ChannelID
  | StartService
  deriving stock (Eq, Show)

data ActionRep =
    ActionList
  | ActionJoin
  | ActionLeave
  | ActionSend
  deriving stock (Eq, Show)

getActionID :: ChatMessage -> ActionID
getActionID s = case s of
    NoOp -> 0
    List n _ -> n
    Join n _ _ -> n
    Leave n _ _ -> n
    Send n _ _ _ -> n
    Share n _ _ _ -> n
    NewJoin n _ _ _ -> n
    NewLeave n _ _ _ -> n
    ChannelList n _ _ -> n
    StartService -> 0
    

type Buffer = Map.Map ChannelID [ChatMessage]

type ClientsState = Map.Map ActorId [ChannelID]

-- type SystemState = (Buffer, ClientsState, Int)

type ChatState = (ClientsState, Int)


