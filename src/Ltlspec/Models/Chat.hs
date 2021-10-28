{-# LANGUAGE OverloadedStrings #-}

module Ltlspec.Models.Chat where

import Data.Aeson (ToJSON (..), (.=), object)
import System.Random (mkStdGen, randomR, StdGen)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List

type ActionID = Integer

type ClientID = Integer

type ChannelName = Integer

type MessageContent = String

data ClientAction =
    List ActionID ClientID
  | Join ActionID ClientID ChannelName
  | Leave ActionID ClientID ChannelName
  | Send ActionID ClientID MessageContent ChannelName
  deriving stock (Eq, Show, Ord)

instance ToJSON ClientAction where
  toJSON = \case
    List aid cid -> object ["type" .= ("list" :: String), "action" .= aid, "client" .= cid]
    _ -> undefined

data ServerResponse =
    Share ActionID MessageContent ClientID
  | NewJoin ActionID ClientID ChannelName ClientID
  | NewLeave ActionID ClientID ChannelName ClientID
  | ChannelList ActionID ClientID ChannelName
  | StartService
  deriving stock (Eq, Show, Ord)

type SystemEvent = Either ClientAction ServerResponse

type SystemTrace = [(State,SystemEvent)]

type State = Map.Map ClientID [ChannelName]

type SystemState = (SystemTrace, Integer)


send :: SystemState -> ClientID -> MessageContent -> ChannelName -> SystemState
send (trace, seed) client message channel = (changeTrace trace, (seed+1))
    where {
        changeTrace t = let newS = fst (last t) in t ++ [(newS,Left (Send (seed + 1) client message channel) )] ++ (map (\r -> (newS,Right (Share (seed +1) message r) ) ) [ c | (c,v)<- Map.toList newS, c/=client, elem channel v ] )
    }

join :: SystemState -> ClientID -> ChannelName -> SystemState
join (trace, seed) client channel = ((changeTrace trace), seed+1)
    where {
        changeState s = Map.insert client  (( filter (/= channel) (Map.findWithDefault [] client s)) ++ [channel] ) s;
        changeTrace t = let 
                            oldS = fst (last t)
                            newS = changeState (oldS) 
                        in t ++ [(newS, Left (Join (seed+1) client channel ) )] ++ (map (\r -> (newS, Right (NewJoin (seed +1) client channel r) ) ) [ c | (c,v)<- Map.toList oldS, c/=client, elem channel v ] )
    }

leave :: SystemState -> ClientID -> ChannelName -> SystemState
leave (trace, seed) client channel = (changeTrace trace, seed+1)
    where {
        changeState s = Map.insert client  (( filter (/= channel) (Map.findWithDefault [] client s))) s;
        changeTrace t = let 
                            oldS = fst (last t)
                            newS = changeState (oldS) 
                        in t ++ [(newS, Left (Leave (seed+1) client channel ))] ++ (map (\r -> (newS, Right (NewLeave (seed +1) client channel r) ) ) [ c | (c,v)<- Map.toList oldS, c/=client, elem channel v ] )
    }

list :: SystemState -> ClientID -> SystemState
list (trace, seed) client = (changeTrace trace, seed+1)
    where {
        changeTrace t = let newS = fst (last t) in t ++ [(newS, Left (List (seed+1) client))] ++ Set.toList (Set.map (\c -> (newS, Right (ChannelList (seed+1) client c))) (Set.fromList (concat (Map.elems newS))) )
    }

getJoinableChannels :: State -> ClientID -> [ChannelName]
getJoinableChannels state client = let res = List.sort (concat (Map.elems state)) in if null res then [0] else res

testState :: Integer -> SystemState
testState nclient= ([((Map.fromList [(i,[]) | i <- [1..nclient]]),Right StartService)], 0)

gen :: StdGen
gen = mkStdGen 137

getRandomElementOfList :: [a] -> StdGen -> a
getRandomElementOfList l gen = let randomIndex = fst (randomR (0, ( (length l) - 1 )) gen) in l !! randomIndex

step :: (SystemState, StdGen) -> String -> (SystemState, StdGen)
step ((trace, seed), gen) actionname =
    let
        randomActionIndex = fst (randomR (0::Int, 3) gen)
        newgen = snd (randomR (0::Int, 3) gen)
        state = fst (last trace)
    in
    case actionname of
        "send" -> let
                    client = getRandomElementOfList (Map.keys state) gen
                  in if null (Map.findWithDefault [] client state) then
                        ((trace, seed), newgen)
                    else
                        let 
                            channel = getRandomElementOfList (Map.findWithDefault [] client state) gen
                            message = "FOO"
                        in
                        (send (trace, seed) client message channel, newgen)
        "join" -> let
                    client = getRandomElementOfList (Map.keys state) gen
                    allchanels = getJoinableChannels state client
                    channel = getRandomElementOfList ( allchanels ++ [(last allchanels) + 1]) gen
                  in (join (trace, seed) client channel, newgen)
        "leave" -> let
                        client = getRandomElementOfList (Map.keys state) gen
                    in if null (Map.findWithDefault [] client state) then
                        ((trace, seed), newgen)
                    else
                        let channel = getRandomElementOfList (Map.findWithDefault [] client state) gen in
                        (leave (trace, seed) client channel, newgen)
        "list" ->  let
                        client = getRandomElementOfList (Map.keys state) gen
                    in (list (trace, seed) client, newgen)

simulationStep :: (SystemState, StdGen) -> (SystemState, StdGen)
simulationStep ((trace, seed), gen) =
    let
        randomActionIndex = fst (randomR (0::Int, 11) gen)
        newgen = snd (randomR (0::Int, 11) gen)
        state = fst (last trace)
    in
    let actionname = ["send","send","send","send","send", "join", "join", "join", "join", "list", "leave", "leave"] !! randomActionIndex in
    case actionname of
        "send" -> let
                    client = getRandomElementOfList (Map.keys state) gen
                  in if null (Map.findWithDefault [] client state) then
                        ((trace, seed), newgen)
                    else
                        let 
                            channel = getRandomElementOfList (Map.findWithDefault [] client state) gen
                            message = "FOO"
                        in
                        (send (trace, seed) client message channel, newgen)
        "join" -> let
                    client = getRandomElementOfList (Map.keys state) gen
                    allchanels = getJoinableChannels state client
                    channel = getRandomElementOfList ( allchanels ++ [(last allchanels) + 1]) gen
                  in (join (trace, seed) client channel, newgen)
        "leave" -> let
                        client = getRandomElementOfList (Map.keys state) gen
                    in if null (Map.findWithDefault [] client state) then
                        ((trace, seed), newgen)
                    else
                        let channel = getRandomElementOfList (Map.findWithDefault [] client state) gen in
                        (leave (trace, seed) client channel, newgen)
        "list" ->  let
                        client = getRandomElementOfList (Map.keys state) gen
                    in (list (trace, seed) client, newgen)

randomTraceGenerator :: Integer -> Integer -> (SystemState, StdGen)
randomTraceGenerator 0 nclients = (testState nclients, gen)
randomTraceGenerator niterations nclients =  simulationStep (randomTraceGenerator (niterations-1) nclients)

singleActionTraceGenerator :: Integer -> Integer -> String -> (SystemState, StdGen)
singleActionTraceGenerator 0 nclients _ = (testState nclients, gen)
singleActionTraceGenerator niterations nclients action =  step (singleActionTraceGenerator (niterations-1) nclients action) action


getTrace :: SystemState -> SystemTrace
getTrace (st, _) = st

trace :: SystemTrace
trace = getTrace (fst (randomTraceGenerator 10000 3) )