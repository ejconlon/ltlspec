{-# LANGUAGE OverloadedStrings #-}

module Ltlspec.Models.Chat where

import Data.Aeson (ToJSON (..), (.=), object)
import System.Random (mkStdGen, randomR, StdGen)
import qualified Data.Map as Map
import qualified Data.Set as Set

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
  deriving stock (Eq, Show, Ord)

type SystemEvent = Either ClientAction ServerResponse

type SystemTrace = [SystemEvent]

type State = Map.Map ClientID [ChannelName]

type SystemState = (State, SystemTrace, Integer)


send :: SystemState -> ClientID -> MessageContent -> ChannelName -> SystemState
send (state, trace, seed) client message channel = (state, (sendHelper trace), (seed+1))
    where {
        sendHelper t = t ++ [(Left (Send (seed + 1) client message channel) )] ++ (map (\r -> (Right (Share (seed +1) message r) ) ) [ c | (c,v)<- Map.toList state, c/=client, elem channel v ] )
    }

join :: SystemState -> ClientID -> ChannelName -> SystemState
join (state, trace, seed) client channel = ((changeState state), (changeTrace trace), seed+1)
    where {
        changeState s = Map.insert client  (( filter (/= channel) (Map.findWithDefault [] client s)) ++ [channel] ) s;
        changeTrace t = t ++ [(Left (Join (seed+1) client channel ) )] ++ (map (\r -> (Right (NewJoin (seed +1) client channel r) ) ) [ c | (c,v)<- Map.toList state, c/=client, elem channel v ] )
    }

leave :: SystemState -> ClientID -> ChannelName -> SystemState
leave (state, trace, seed) client channel = (changeState state, changeTrace trace, seed+1)
    where {
        changeState s = Map.insert client  (( filter (/= channel) (Map.findWithDefault [] client s))) s;
        changeTrace t = t ++ [(Left (Leave (seed+1) client channel ))] ++ (map (\r -> (Right (NewLeave (seed +1) client channel r) ) ) [ c | (c,v)<- Map.toList state, c/=client, elem channel v ] )
    }

list :: SystemState -> ClientID -> SystemState
list (state, trace, seed) client = (state, changeTrace trace, seed+1)
    where {
        changeTrace t = t ++ [(Left (List (seed+1) client))] ++ Set.toList (Set.map (\c -> (Right (ChannelList (seed+1) client c))) (Set.fromList (concat (Map.elems state))) )
    }

getJoinableChannels :: State -> ClientID -> [ChannelName]
getJoinableChannels state client = let res = concat (Map.elems state) in if null res then [0] else res

testState :: Integer -> SystemState
testState nclient= ((Map.fromList [(i,[]) | i <- [1..nclient]]),[], 0)

gen :: StdGen
gen = mkStdGen 137

getRandomElementOfList :: [a] -> StdGen -> a
getRandomElementOfList l gen = let randomIndex = fst (randomR (0, ( (length l) - 1 )) gen) in l !! randomIndex


simulationStep :: (SystemState, StdGen) -> (SystemState, StdGen)
simulationStep ((state, trace, seed), gen) =
    let
        randomActionIndex = fst (randomR (0::Int, 2) gen)
        newgen = snd (randomR (0::Int, 2) gen)
    in
    let actionname = ["send", "join", "list"] !! randomActionIndex in
    case actionname of
        "send" -> let
                    client = getRandomElementOfList (Map.keys state) gen
                  in if null (Map.findWithDefault [] client state) then
                        ((state, trace, seed), newgen)
                    else
                        let 
                            channel = getRandomElementOfList (Map.findWithDefault [] client state) gen
                            message = "FOO"
                        in
                        (send (state, trace, seed) client message channel, newgen)
        "join" -> let
                    client = getRandomElementOfList (Map.keys state) gen
                    allchanels = getJoinableChannels state client
                    channel = getRandomElementOfList ( allchanels ++ [last allchanels + 1]) gen
                  in (join (state, trace, seed) client channel, newgen)
        "leave" -> let
                        client = getRandomElementOfList (Map.keys state) gen
                    in if null (Map.findWithDefault [] client state) then
                        ((state, trace, seed), newgen)
                    else
                        let channel = getRandomElementOfList (Map.findWithDefault [] client state) gen in
                        (leave (state, trace, seed) client channel, newgen)
        "list" ->  let
                        client = getRandomElementOfList (Map.keys state) gen
                    in (list (state, trace, seed) client, newgen)

generator :: Integer -> Integer -> (SystemState, StdGen)
generator 0 nclients = (testState nclients, gen)
generator niterations nclients =  simulationStep (generator (niterations-1) nclients)

getTrace :: SystemState -> SystemTrace
getTrace (_, st, _) = st

trace :: SystemTrace
trace = getTrace (fst (generator 100 3) )