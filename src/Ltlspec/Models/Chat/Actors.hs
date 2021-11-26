module Ltlspec.Models.Chat.Actors where
-- import Ltlspec.System.Actors (ActorId, Behavior, TickMessage (TickMessageFire, TickMessageEmbed), AppMessage (AppMessage), ActorConstructor)
-- import Ltlspec.System.Time (TimeDelta)
-- import qualified Data.Map as Map

-- type ActionID = Integer
-- type ClientID = Integer
-- type ChannelID = Integer
-- type MessageContent = String

-- data ChatMessage =
--   NoOp
--   | List ActionID ClientID
--   | Join ActionID ClientID ChannelID
--   | Leave ActionID ClientID ChannelID
--   | Send ActionID ClientID MessageContent ChannelID
--   | Share ActionID ClientID MessageContent ClientID
--   | NewJoin ActionID ClientID ChannelID ClientID
--   | NewLeave ActionID ClientID ChannelID ClientID
--   | ChannelList ActionID ClientID ChannelID
--   | StartService
--   deriving stock (Eq, Show)

-- type ServerState = Map.Map ClientID [ChannelID]
-- type ClientState = [ChannelID]

-- data ChatConfig =
--   Client ClientState
--   | Server ServerState
--   deriving stock (Eq, Show)

-- mkChatConfigs :: Int -> [ChatConfig]
-- mkChatConfigs nclients = Server : [Client | _<-[1..nclients]]

-- chatConfigs :: [ChatConfig]
-- chatConfigs = mkChatConfigs 5

-- chatBehavior :: ChatConfig -> [ActorId] -> Behavior (TickMessage ChatMessage)
-- chatBehavior role recvs hand (AppMessage sendAid tm) =
--   case role of
--     Client channels ->
--       case tm of
--         TickMessageFire -> error "TODO pick a random action (list/join/leave/send)"
--         TickMessageEmbed cm ->
--           case cm of
--             Share n i s j -> undefined
--             NewJoin n i j x -> undefined
--             NewLeave n i j x -> undefined
--             ChannelList n i j -> undefined
--             StartService -> undefined
--             _ -> error (show cm ++ "is not a valid client message")
--     Server clientsS ->
--         case tm of
--           TickMessageFire -> error "Server should not receive fire messages"
--           TickMessageEmbed cm ->
--             case cm of
--               NoOp -> undefined
--               List n i -> undefined
--               Join n i j -> undefined
--               Leave n i j -> undefined
--               Send n i s j -> undefined
--               _ -> error (show cm ++ "is not a valid server message")
              
-- chatCtor :: Int -> TimeDelta -> IO (ActorConstructor ChatConfig (TickMessage ChatMessage))
-- chatCtor limit interval = do
--   channels 