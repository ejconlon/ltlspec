{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Ltlspec.Models.Chat.Actors where
import Ltlspec.System.Actors (ActorId (ActorId), Behavior, TickMessage (TickMessageFire, TickMessageEmbed), AppMessage (AppMessage), ActorConstructor, mkTickConfig, ActorCase (ActorCase), minimalTickMessageFilter, AnnoMessage, runActorCaseSimple)
import Ltlspec.System.Time (TimeDelta, timeDeltaFromFracSecs)
import qualified Data.Map as Map
import Control.Concurrent.STM (TVar, newTVarIO, writeTVar, readTVar)
import Control.Monad (replicateM)
import System.Random (StdGen, mkStdGen, randomR)
import qualified Data.Set as Set
import Data.Hashable ( Hashable )
import Control.DeepSeq (NFData)
import Ltlspec.System.Logging (consoleLogger, Logger)
import Text.Pretty.Simple (pPrint)
import Ltlspec.Models.Chat.Commons (ChatMessage(..), ChannelID(..), ActionID(..))


type MessageContent = String
type ServerState = Map.Map ActorId [ChannelID]
type ClientState = ([ChannelID],[ChannelID], StdGen, Int)
data ActionRep =
    ActionList
  | ActionJoin
  | ActionLeave
  | ActionSend
  deriving stock (Eq, Show)

data ChatConfig =
  Client ActorId (TVar ClientState)
  | Server (TVar ServerState)
  deriving stock (Eq)

instance Show ChatConfig where
    show = \case
      Client n _ -> "Client: " ++ show n
      Server _ -> "Server"

-- data ChatMessage =
--   NoOp
--   | List ActionID ActorId
--   | Join ActionID ActorId ChannelID
--   | Leave ActionID ActorId ChannelID
--   | Send ActionID ActorId MessageContent ChannelID
--   | Share ActionID ActorId MessageContent ActorId
--   | NewJoin ActionID ActorId ChannelID ActorId
--   | NewLeave ActionID ActorId ChannelID ActorId
--   | ChannelList ActionID ActorId ChannelID
--   | StartService
--   deriving stock (Eq, Show)

getRandomElementOfList :: [a] -> StdGen -> a
getRandomElementOfList l gen = let randomIndex = fst (randomR (0, length l - 1) gen) in l !! randomIndex

mkChatConfigs_ :: Int -> [TVar ClientState] -> TVar ServerState -> [ChatConfig]
mkChatConfigs_ nc css ss = Server ss : [Client (ActorId (i+1)) (css !! i) | i <-[0..(nc-1)]]

mkChatConfigs :: Int -> IO [ChatConfig]
mkChatConfigs nclients =
    do
      css <- replicateM nclients (newTVarIO ([],[], mkStdGen 137, 1))
      ss <- newTVarIO (Map.fromList ([ (ActorId k, []) | k <- [1..nclients] ]))
      return $ mkChatConfigs_ nclients css ss

chatConfigs :: IO [ChatConfig]
chatConfigs = mkChatConfigs 5

chatBehavior :: ChatConfig -> Behavior (TickMessage ChatMessage)
chatBehavior (Client cid channelsAndRdm) sendMsg (AppMessage _ tm) =
  case tm of
    TickMessageFire ->
      do
        (mychannels,knownchannels, gen, seed) <- readTVar channelsAndRdm
        let
          (randomActionIndex, newgen) = randomR (0, 11) gen
          actionname = [ActionSend,ActionSend,ActionSend,ActionSend,ActionSend, ActionJoin, ActionJoin, ActionJoin, ActionJoin, ActionList, ActionLeave, ActionLeave] !! randomActionIndex
          go act =
            case act of
              ActionSend -> if null mychannels then
                                 do
                                   writeTVar channelsAndRdm (mychannels,knownchannels, newgen, seed+1)
                                   return ()
                            else
                                let
                                    channel = getRandomElementOfList mychannels gen
                                    message = "FOO"
                                in
                                  do
                                    writeTVar channelsAndRdm (mychannels,knownchannels, newgen, seed+1)
                                    sendMsg (AppMessage 0 (TickMessageEmbed (Send (ActionID (seed+1)) cid message channel) ))
              ActionJoin ->   if null knownchannels then
                                do
                                  writeTVar channelsAndRdm (mychannels,knownchannels, newgen, seed+1)
                                  sendMsg (AppMessage 0 (TickMessageEmbed (List (ActionID (seed+1)) cid) ))
                              else
                                let
                                  joinableChannels = filter (`notElem` mychannels) knownchannels
                                in
                                  if null joinableChannels then
                                    do
                                      writeTVar channelsAndRdm (mychannels,knownchannels, newgen, seed+1)
                                      sendMsg (AppMessage 0 (TickMessageEmbed (List (ActionID (seed+1)) cid) ))
                                  else
                                    let
                                      channel = getRandomElementOfList joinableChannels gen
                                    in
                                      do
                                        writeTVar channelsAndRdm (mychannels ++ [channel], knownchannels, newgen, seed+1)
                                        sendMsg (AppMessage 0 (TickMessageEmbed (Join (ActionID (seed+1)) cid channel) ))
              ActionLeave ->  if null mychannels then
                                 do
                                   writeTVar channelsAndRdm (mychannels,knownchannels, newgen, seed+1)
                                   return ()
                              else
                                let
                                    channel = getRandomElementOfList mychannels gen
                                in
                                  do
                                    writeTVar channelsAndRdm (filter (/= channel) mychannels,knownchannels, newgen, seed+1)
                                    sendMsg (AppMessage 0 (TickMessageEmbed (Leave (ActionID (seed+1)) cid channel) ))
              ActionList ->   do
                                writeTVar channelsAndRdm (mychannels,knownchannels, newgen, seed+1)
                                sendMsg (AppMessage 0 (TickMessageEmbed (List (ActionID (seed+1)) cid) ))
          in
          go actionname
    TickMessageEmbed cm -> case cm of
      ChannelList _ _ ci ->
        do
          (mychannels,knownchannels, gen, seed) <- readTVar channelsAndRdm
          let newknownchannels = filter (/= ci) knownchannels ++ [ci] in
            writeTVar channelsAndRdm (mychannels,newknownchannels, gen, seed)
      _ -> return ()

chatBehavior (Server channelsMap) sendMsg (AppMessage sendAid tm) =
  case tm of
    TickMessageFire -> error "Server should not receive fire messages"
    TickMessageEmbed cm ->
      case cm of
        NoOp -> return ()
        List n i ->
          do
            cState <- readTVar channelsMap
            let allchannels = Set.toList (Set.fromList (concat (Map.elems cState))) in listChannels allchannels
          where
            listChannels [] = return()
            listChannels (ch:chs) =
              do
                sendMsg (AppMessage sendAid (TickMessageEmbed (ChannelList n i ch)))
                listChannels chs

        Join n client channel ->
          do
            cState <- readTVar channelsMap
            writeTVar channelsMap (Map.insert client  (filter (/= channel) (Map.findWithDefault [] client cState) ++ [channel] ) cState)
            notifyNewJoin [NewJoin n client channel c | (c, v) <- Map.toList cState, c /= client, channel `elem` v]
          where
            notifyNewJoin [] = return()
            notifyNewJoin ((NewJoin n client channel c):msgs) =
              do
                sendMsg (AppMessage c (TickMessageEmbed (NewJoin n client channel c)))
                notifyNewJoin msgs

        Leave n client channel ->
          do
            cState <- readTVar channelsMap
            writeTVar channelsMap (Map.insert client  (filter (/= channel) (Map.findWithDefault [] client cState)) cState)
            notifyNewLeave [NewLeave n client channel c | (c, v) <- Map.toList cState, c /= client, channel `elem` v]
          where
            notifyNewLeave [] = return()
            notifyNewLeave ((NewLeave n client channel c):msgs) =
              do
                sendMsg (AppMessage c (TickMessageEmbed (NewLeave n client channel c)))
                notifyNewLeave msgs

        Send n client s channel ->
          do
            cState <- readTVar channelsMap
            shareMsg [Share n client s c | (c, v) <- Map.toList cState, c /= client, channel `elem` v]
          where
            shareMsg [] = return()
            shareMsg ((Share n client content c):msgs) =
              do
                sendMsg (AppMessage c (TickMessageEmbed (Share n client content c)))
                shareMsg msgs
        _ -> error (show cm ++ "is not a valid server message")

chatCtor :: Int -> TimeDelta -> ActorConstructor ChatConfig (TickMessage ChatMessage)
chatCtor limit interval _ myId myRole = case myRole of
  Client _ _ -> let tick = mkTickConfig Nothing interval (Just limit) myId in ([tick],chatBehavior myRole)
  Server _ -> ([],chatBehavior myRole)

chatCase :: Int -> TimeDelta -> [ChatConfig]-> ActorCase ChatMessage
chatCase limit interval pairs = ActorCase (chatCtor limit interval) pairs minimalTickMessageFilter

runChatSim :: Logger -> Int -> TimeDelta -> [ChatConfig] -> IO [AnnoMessage ChatMessage]
runChatSim logger limit interval pairs = runActorCaseSimple logger (chatCase limit interval pairs)

main :: IO ()
main = do 
  logger <- consoleLogger
  configs <- chatConfigs
  messages <- runChatSim logger 10 (timeDeltaFromFracSecs (1 :: Double)) configs
  pPrint messages