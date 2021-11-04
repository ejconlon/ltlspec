{-# LANGUAGE OverloadedStrings #-}

module Ltlspec.Models.Chat where

import Data.Aeson (ToJSON (..), object, (.=))
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Ltlspec (Atom (Atom), Prop (PropAnd, PropAtom, PropNot, PropUntil), Theory (..), propAlways, propAndAll,
                propEventually, propExistsNested, propForAllNested, propIf)
import System.Random (StdGen, mkStdGen, randomR)

chatTheory :: Theory
chatTheory = Theory
  { theoryTypes = ["ClientID", "ChannelID", "ActionID"]
  , theoryProps = Map.fromList [("IsMember", ["ClientID", "ChannelID"]),
                                ("IsSameClient", ["ClientID", "ClientID"]),
                                ("Left", ["ActionID", "ClientID", "ChannelID"]),
                                ("Joined", ["ActionID", "ClientID", "ChannelID"]),
                                ("ListRequested",["ActionID", "ClientID"]),
                                ("Sent", ["ActionID", "ClientID", "ChannelID"]),
                                ("Shared", ["ActionID", "ClientID", "ClientID"]),
                                ("NewJoinNote", ["ActionID", "ClientID", "ChannelID", "ClientID"]),
                                ("NewLeaveNote", ["ActionID", "ClientID", "ChannelID", "ClientID"]),
                                ("ChannelListNote", ["ActionID", "ClientID", "ChannelID"])]
  , theoryAxioms = Map.fromList [
        ("IsMemberBetweenJoinAndLeave",
            propAlways (
                propForAllNested [("c","ClientID"), ("ch", "ChannelID")] (
                    propExistsNested [("i","ActionID"), ("j","ActionID")] (
                        propIf
                            (PropAtom (Atom "Joined" ["i","c","ch"]))
                            (PropUntil
                                (PropAtom (Atom "IsMember" ["c","ch"]))
                                (PropAtom (Atom "Left" ["j","c","ch"]))
                            )
                    )
                )
            )
        ),
        ("IfInChannelReceiveMessage",
            propAlways (
                propForAllNested [("c1","ClientID"),("ch","ChannelID"),("c2","ClientID"),("m","ActionID")] (
                    propIf
                        (propAndAll [
                            PropNot (PropAtom (Atom "IsSameClient" ["c1","c2"])),
                            PropAtom (Atom "IsMember" ["c1","ch"]),
                            PropAtom (Atom "IsMember" ["c2","ch"]),
                            PropAtom (Atom "Sent" ["m","c1","ch"])
                        ])
                        (PropAnd
                            (PropNot (PropAtom (Atom "Shared" ["m","c1","c1"])))
                            (propEventually (PropAtom (Atom "Shared" ["m", "c1", "c2"])))
                        )
                )
            )
        ),
        ("NeverSendMessageToMyself",
            propAlways (
                propForAllNested [("c","ClientID"), ("m", "ActionID")] (
                    PropNot (PropAtom (Atom "Shared" ["m", "c", "c"]))
                )
            )
        )
    ]
  }



type ActionID = Integer
type ClientID = Integer
type ChannelID = Integer
type MessageContent = String

data ClientAction =
    List ActionID ClientID
  | Join ActionID ClientID ChannelID
  | Leave ActionID ClientID ChannelID
  | Send ActionID ClientID MessageContent ChannelID
  deriving stock (Eq, Show, Ord)

data ActionRep =
    ActionList
    | ActionJoin
    | ActionLeave
    | ActionSend

instance ToJSON ClientAction where
  toJSON = \case
    List aid cid -> object ["type" .= ("list" :: String), "action" .= aid, "client" .= cid]
    Join aid cid cn -> object ["type" .= ("join" :: String), "action" .= aid, "client" .= cid, "channel" .= cn]
    Leave aid cid cn -> object ["type" .= ("leave" :: String), "action" .= aid, "client" .= cid, "channel" .= cn]
    Send aid cid msg cn -> object ["type" .= ("send" :: String), "action" .= aid, "client" .= cid, "message" .= msg, "channel" .= cn]


data ServerResponse =
    Share ActionID ClientID MessageContent ClientID
  | NewJoin ActionID ClientID ChannelID ClientID
  | NewLeave ActionID ClientID ChannelID ClientID
  | ChannelList ActionID ClientID ChannelID
  | StartService
  deriving stock (Eq, Show, Ord)

instance ToJSON ServerResponse where
  toJSON = \case
    ChannelList aid cid cn -> object ["type" .= ("channel-list" :: String), "correspondent-action" .= aid, "client" .= cid, "channel" .= cn]
    NewJoin aid sid cn rid -> object ["type" .= ("new-join" :: String), "correspondent-action" .= aid, "sender" .= sid, "channel" .= cn, "receiver" .= rid]
    NewLeave aid sid cn rid -> object ["type" .= ("new-leave" :: String), "correspondent-action" .= aid, "sender" .= sid, "channel" .= cn, "receiver" .= rid]
    Share aid sid msg rid -> object ["type" .= ("share" :: String), "correspondent-action" .= aid, "sender" .= sid, "message" .= msg, "receiver" .= rid]
    StartService -> object ["type" .= ("start-service"::String) ]


type SystemEvent = Either ClientAction ServerResponse

type SystemTrace = [(State,SystemEvent)]

type State = Map.Map ClientID [ChannelID]

type SystemState = (SystemTrace, Integer)


send :: SystemState -> ClientID -> MessageContent -> ChannelID -> SystemState
send (trace, seed) client message channel = (changeTrace trace, seed+1)
    where {
        changeTrace t = let newS = fst (last t) in t ++ [(newS,Left (Send (seed + 1) client message channel) )] ++ [(\ r -> (newS, Right (Share (seed + 1) client message r))) c |
                           (c, v) <- Map.toList newS, c /= client, channel `elem` v]
    }

join :: SystemState -> ClientID -> ChannelID -> SystemState
join (trace, seed) client channel = (changeTrace trace, seed+1)
    where {
        changeState s = Map.insert client  (filter (/= channel) (Map.findWithDefault [] client s) ++ [channel] ) s;
        changeTrace t = let
                            oldS = fst (last t)
                            newS = changeState oldS
                        in t ++ [(newS, Left (Join (seed+1) client channel ) )] ++ [(\ r -> (newS, Right (NewJoin (seed + 1) client channel r))) c |
                           (c, v) <- Map.toList oldS, c /= client, channel `elem` v]
    }

leave :: SystemState -> ClientID -> ChannelID -> SystemState
leave (trace, seed) client channel = (changeTrace trace, seed+1)
    where {
        changeState s = Map.insert client  (filter (/= channel) (Map.findWithDefault [] client s)) s;
        changeTrace t = let
                            oldS = fst (last t)
                            newS = changeState oldS
                        in t ++ [(newS, Left (Leave (seed+1) client channel ))] ++ [(\ r -> (newS, Right (NewLeave (seed + 1) client channel r))) c |
                           (c, v) <- Map.toList oldS, c /= client, channel `elem` v]
    }

list :: SystemState -> ClientID -> SystemState
list (trace, seed) client = (changeTrace trace, seed+1)
    where {
        changeTrace t = let newS = fst (last t) in t ++ [(newS, Left (List (seed+1) client))] ++ Set.toList (Set.map (\c -> (newS, Right (ChannelList (seed+1) client c))) (Set.fromList (concat (Map.elems newS))) )
    }

getJoinableChannels :: State -> ClientID -> [ChannelID]
getJoinableChannels state client = let res = List.sort (concat ([v | (k,v) <- Map.toList state, k /= client  ])) in if null res then [0] else res

initialState :: Integer -> SystemState
initialState nclient= ([(Map.fromList [(i,[]) | i <- [1..nclient]],Right StartService)], 0)

randomGen :: StdGen
randomGen = mkStdGen 137

getRandomElementOfList :: [a] -> StdGen -> a
getRandomElementOfList l gen = let randomIndex = fst (randomR (0, length l - 1) gen) in l !! randomIndex

-- TODO(tarcisio) Decompose to two functions, ChatState -> StdGen -> (ChatMessage, StdGen), and ChatMessage -> ChatState -> ChatState
step :: (SystemState, StdGen) -> ActionRep -> (SystemState, StdGen)
step ((trace, seed), gen) actionname =
    let
        newgen = snd (randomR (0::Int, 3) gen)
        state = fst (last trace)
    in
    case actionname of
        ActionSend -> let
                    client = getRandomElementOfList (Map.keys state) gen
                  in if null (Map.findWithDefault [] client state) then
                        ((trace, seed), newgen)
                    else
                        let
                            channel = getRandomElementOfList (Map.findWithDefault [] client state) gen
                            message = "FOO"
                        in
                        (send (trace, seed) client message channel, newgen)
        ActionJoin -> let
                    client = getRandomElementOfList (Map.keys state) gen
                    allchanels = getJoinableChannels state client
                    channel = getRandomElementOfList ( allchanels ++ [last allchanels + 1]) gen
                  in (join (trace, seed) client channel, newgen)
        ActionLeave -> let
                        client = getRandomElementOfList (Map.keys state) gen
                    in if null (Map.findWithDefault [] client state) then
                        ((trace, seed), newgen)
                    else
                        let channel = getRandomElementOfList (Map.findWithDefault [] client state) gen in
                        (leave (trace, seed) client channel, newgen)
        ActionList ->  let
                        client = getRandomElementOfList (Map.keys state) gen
                    in (list (trace, seed) client, newgen)

simulationStep :: (SystemState, StdGen) -> (SystemState, StdGen)
simulationStep ((trace, seed), gen) =
    let
        randomActionIndex = fst (randomR (0::Int, 11) gen)
        newgen = snd (randomR (0::Int, 11) gen)
        state = fst (last trace)
    in
    let actionname = [ActionSend,ActionSend,ActionSend,ActionSend,ActionSend, ActionJoin, ActionJoin, ActionJoin, ActionJoin, ActionList, ActionLeave, ActionLeave] !! randomActionIndex in
    case actionname of
        ActionSend -> let
                    client = getRandomElementOfList (Map.keys state) gen
                  in if null (Map.findWithDefault [] client state) then
                        ((trace, seed), newgen)
                    else
                        let
                            channel = getRandomElementOfList (Map.findWithDefault [] client state) gen
                            message = "FOO"
                        in
                        (send (trace, seed) client message channel, newgen)
        ActionJoin -> let
                    client = getRandomElementOfList (Map.keys state) gen
                    allchanels = getJoinableChannels state client
                    channel = getRandomElementOfList ( allchanels ++ [last allchanels + 1]) gen
                  in (join (trace, seed) client channel, newgen)
        ActionLeave -> let
                        client = getRandomElementOfList (Map.keys state) gen
                    in if null (Map.findWithDefault [] client state) then
                        ((trace, seed), newgen)
                    else
                        let channel = getRandomElementOfList (Map.findWithDefault [] client state) gen in
                        (leave (trace, seed) client channel, newgen)
        ActionList ->  let
                        client = getRandomElementOfList (Map.keys state) gen
                    in (list (trace, seed) client, newgen)

randomTraceGenerator :: Integer -> Integer -> (SystemState, StdGen)
randomTraceGenerator 0 nclients = (initialState nclients, randomGen)
randomTraceGenerator niterations nclients =  simulationStep (randomTraceGenerator (niterations-1) nclients)

singleActionTraceGenerator :: Integer -> Integer -> ActionRep -> (SystemState, StdGen)
singleActionTraceGenerator 0 nclients _ = (initialState nclients, randomGen)
singleActionTraceGenerator niterations nclients action =  step (singleActionTraceGenerator (niterations-1) nclients action) action


getTrace :: SystemState -> SystemTrace
getTrace (st, _) = st

-- TODO(tarcisio) Emit list of ChatWorld = SAS ChatMessage ChatState
sampleTrace :: SystemTrace
sampleTrace = getTrace (fst (randomTraceGenerator 5 3) )

-- TODO(tarcisio) Implement bridge for this theory with unit tests
