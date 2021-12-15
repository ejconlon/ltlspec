module Ltlspec.Models.Chat.Chat where

-- import qualified Data.Bifunctor
-- import qualified Data.List as List
-- import qualified Data.Map as Map
-- import qualified Data.Set as Set
-- import Ltlspec.TriBool (TriBool (..))
-- import Ltlspec.Types (Atom (..), BinderGroup (..), Bridge (..), Commented (..), Error, Prop (..), SAS (..), SProp (..),
--                       Theory (..), TruncBridge, initScanSAS, truncBridgeEmpty, truncBridgeOracle)
-- import System.Random (StdGen, mkStdGen, randomR)

-- chatTheory :: Theory
-- chatTheory = Theory
--   { theoryTypes = NoComment <$> ["Client", "Channel", "Action"]
--   , theoryProps = NoComment <$> Map.fromList
--       [ ("IsMember", ["Client", "Channel"])
--       , ("IsSameClient", ["Client", "Client"])
--       , ("Left", ["Action", "Client", "Channel"])
--       , ("Joined", ["Action", "Client", "Channel"])
--       , ("ListRequested",["Action", "Client"])
--       , ("Sent", ["Action", "Client", "Channel"])
--       , ("Shared", ["Action", "Client", "Client"])
--       , ("NewJoinNote", ["Action", "Client", "Channel", "Client"])
--       , ("NewLeaveNote", ["Action", "Client", "Channel", "Client"])
--       , ("ChannelListNote", ["Action", "Client", "Channel"])
--       ]
--   , theoryAxioms = NoComment <$> Map.fromList
--       [ ("isMemberBetweenJoinAndLeave",
--           SPropAlways (
--             SPropForAll [BinderGroup ["c"] "Client", BinderGroup ["ch"] "Channel"] (
--               SPropExists [BinderGroup ["i"] "Action"] (
--                 SPropIf
--                   [SPropAtom (Atom "Joined" ["i", "c", "ch"])]
--                   (SPropExists [BinderGroup ["j"] "Action"] (
--                     SPropUntil
--                       (SPropAtom (Atom "IsMember" ["c","ch"]))
--                       (SPropAtom (Atom "Left" ["j","c","ch"]))
--                   )
--                   )
--               )
--             )
--           )
--         )
--         -- ("isMemberBetweenJoinAndLeave",
--         --   SPropAlways (
--         --     SPropForAll [BinderGroup ["c"] "Client", BinderGroup ["ch"] "Channel"] (
--         --       SPropExists [BinderGroup ["i", "j"] "Action"] (
--         --         SPropIf
--         --           [SPropAtom (Atom "Joined" ["i", "c", "ch"])]
--         --           (SPropUntil
--         --             (SPropAtom (Atom "IsMember" ["c","ch"]))
--         --             (SPropAtom (Atom "Left" ["j","c","ch"]))
--         --           )
--         --       )
--         --     )
--         --   )
--         -- )
--       , ("ifInChannelReceiveMessage",
--           SPropAlways (
--             SPropForAll [BinderGroup ["c1", "c2"] "Client", BinderGroup ["ch"] "Channel", BinderGroup ["m"] "Action"] (
--               SPropIf
--                 [ SPropAnd
--                   [ SPropNot (SPropAtom (Atom "IsSameClient" ["c1", "c2"]))
--                   , SPropAtom (Atom "IsMember" ["c1", "ch"])
--                   , SPropAtom (Atom "IsMember" ["c2", "ch"])
--                   , SPropAtom (Atom "Sent" ["m", "c1", "ch"])
--                   ]
--                 ]
--                 (SPropEventually (SPropAtom (Atom "Shared" ["m", "c1", "c2"])))
--             )
--           )
--         )
--       ,
--       ("neverSendMessageToMyself",
--           SPropAlways (
--             SPropForAll [BinderGroup ["c"] "Client", BinderGroup ["m"] "Action"] (
--               SPropNot (SPropAtom (Atom "Shared" ["m", "c", "c"]))
--             )
--           )
--         )
--       ]
--   }

-- type ActionID = Integer
-- type ClientID = Integer
-- type ChannelID = Integer
-- type MessageContent = String

-- data ClientAction =
--     NoOp
--   | List ActionID ClientID
--   | Join ActionID ClientID ChannelID
--   | Leave ActionID ClientID ChannelID
--   | Send ActionID ClientID MessageContent ChannelID
--   deriving stock (Eq, Show)

-- data ActionRep =
--     ActionList
--   | ActionJoin
--   | ActionLeave
--   | ActionSend
--   deriving stock (Eq, Show)

-- data ServerResponse =
--     Share ActionID ClientID MessageContent ClientID
--   | NewJoin ActionID ClientID ChannelID ClientID
--   | NewLeave ActionID ClientID ChannelID ClientID
--   | ChannelList ActionID ClientID ChannelID
--   | StartService
--   deriving stock (Eq, Show, Ord)

-- type SystemEvent = Either ClientAction ServerResponse

-- getActionID :: SystemEvent -> ActionID
-- getActionID s = case s of
--   Left ca -> case ca of
--     NoOp -> 0
--     List n _ -> n
--     Join n _ _ -> n
--     Leave n _ _ -> n
--     Send n _ _ _ -> n
--   Right sr -> case sr of
--     Share n _ _ _ -> n
--     NewJoin n _ _ _ -> n
--     NewLeave n _ _ _ -> n
--     ChannelList n _ _ -> n
--     StartService -> 0


-- type Buffer = Map.Map ChannelID [SystemEvent]

-- type ClientsState = Map.Map ClientID [ChannelID]

-- type SystemState = (Buffer, ClientsState, Integer)

-- type ChatState = (ClientsState, Integer)

-- type ChatWorld = SAS ChatState SystemEvent

-- data ChatVal =
--     ChatValClient ClientID
--   | ChatValChannel ChannelID
--   | ChatValAction ActionID
--   deriving stock (Eq, Show)

-- send :: SystemState -> ClientID -> MessageContent -> ChannelID -> SystemState
-- send (buffer, cState, seed) client message channel = (updateBuffer, cState , seed+1)
--     where {
--         updateBuffer = let oldL = Map.findWithDefault [] channel buffer in
--             Map.insert channel (oldL ++ [Left (Send (seed + 1) client message channel)] ++ [( Right . Share (seed + 1) client message) c |
--                            (c, v) <- Map.toList cState, c /= client, channel `elem` v]) buffer
--     }

-- join :: SystemState -> ClientID -> ChannelID -> SystemState
-- join (buffer, cState, seed) client channel = (updateBuffer, changeState cState, seed+1)
--     where {
--         changeState s = Map.insert client  (filter (/= channel) (Map.findWithDefault [] client s) ++ [channel] ) s;
--         updateBuffer = let oldL = Map.findWithDefault [] channel buffer in
--                         Map.insert channel (oldL ++ [Left (Join (seed+1) client channel)] ++ [( Right . NewJoin (seed + 1) client channel) c |
--                            (c, v) <- Map.toList cState, c /= client, channel `elem` v]) buffer
--     }

-- leave :: SystemState -> ClientID -> ChannelID -> SystemState
-- leave (buffer, cState, seed) client channel = (updateBuffer, changeState cState, seed+1)
--     where {
--         changeState s = Map.insert client  (filter (/= channel) (Map.findWithDefault [] client s) ++ [channel] ) s;
--         updateBuffer = let oldL = Map.findWithDefault [] channel buffer in
--                         Map.insert channel (oldL ++ [Left (Leave (seed+1) client channel )] ++ [( Right . NewLeave (seed + 1) client channel) c |
--                            (c, v) <- Map.toList cState, c /= client, channel `elem` v]) buffer
--     }

-- list :: SystemState -> ClientID -> SystemState
-- list (buffer, cState, seed) client = (updateBuffer, cState, seed+1)
--     where {
--         insertRightBuffer ch m = let oldL = Map.findWithDefault [] ch m in Map.insert ch (oldL ++ [Right (ChannelList (seed+1) client ch)]) m;
--         updateBuffer = Set.fold insertRightBuffer buffer (Set.fromList (concat (Map.elems cState)))
--     }

-- getJoinableChannels :: ClientsState -> ClientID -> [ChannelID]
-- getJoinableChannels state client = let res = List.sort (concat ([v | (k,v) <- Map.toList state, k /= client  ])) in if null res then [0] else res

-- initialState :: Integer -> SystemState
-- initialState nclient = (Map.empty, Map.fromList [(i,[]) | i <- [1..nclient]], 0)

-- initialChatState :: Integer -> ChatState
-- initialChatState nclients = (Map.fromList [(i,[]) | i <- [1..nclients]], 0)

-- randomGen :: StdGen
-- randomGen = mkStdGen 137

-- getRandomElementOfList :: [a] -> StdGen -> a
-- getRandomElementOfList l gen = let randomIndex = fst (randomR (0, length l - 1) gen) in l !! randomIndex

-- processEvent :: ChatState -> SystemEvent -> ChatState
-- processEvent (cState, seed) event =
--     case event of
--         Left NoOp -> (cState, seed)
--         Left (List aid _) -> (cState, aid)
--         Left (Join aid cid chid) -> (Map.insert cid  (filter (/= chid) (Map.findWithDefault [] cid cState) ++ [chid] ) cState, aid)
--         Left (Leave aid cid chid) -> (Map.insert cid  (filter (/= chid) (Map.findWithDefault [] cid cState)) cState, aid)
--         Left (Send aid _ _ _) -> (cState, aid)
--         Right (Share aid _ _ _) -> (cState, aid)
--         Right (NewJoin aid _ _ _) -> (cState, aid)
--         Right (NewLeave aid _ _ _) -> (cState, aid)
--         Right (ChannelList aid _ _) -> (cState, aid)
--         Right StartService -> (cState, seed)

-- simulationStep :: (SystemState, StdGen) -> (SystemState, StdGen)
-- simulationStep ((buffer, cState, seed), gen) =
--     let
--         randomActionIndex = fst (randomR (0::Int, 11) gen)
--         newgen = snd (randomR (0::Int, 11) gen)
--     in
--     let actionname = [ActionSend,ActionSend,ActionSend,ActionSend,ActionSend, ActionJoin, ActionJoin, ActionJoin, ActionJoin, ActionList, ActionLeave, ActionLeave] !! randomActionIndex in
--     case actionname of
--         ActionSend ->   let
--                             client = getRandomElementOfList (Map.keys cState) gen
--                             channels = Map.findWithDefault [] client cState
--                         in if null channels then
--                                 ((buffer, cState, seed), newgen)
--                             else
--                                 let
--                                     channel = getRandomElementOfList (Map.findWithDefault [] client cState) gen
--                                     message = "FOO"
--                                 in
--                                     (send (buffer, cState, seed) client message channel, newgen)
--         ActionJoin ->   let
--                             client = getRandomElementOfList (Map.keys cState) gen
--                             allchanels = getJoinableChannels cState client
--                             channel = getRandomElementOfList ( allchanels ++ [last allchanels + 1]) gen
--                         in (join (buffer, cState, seed) client channel, newgen)
--         ActionLeave ->  let
--                             client = getRandomElementOfList (Map.keys cState) gen
--                             channels = Map.findWithDefault [] client cState
--                         in if null channels then
--                             ((buffer, cState, seed), newgen)
--                         else
--                             let channel = getRandomElementOfList channels gen in
--                                 (leave (buffer, cState, seed) client channel, newgen)
--         ActionList ->   let
--                             client = getRandomElementOfList (Map.keys cState) gen
--                         in (list (buffer, cState, seed) client, newgen)


-- simulation :: Integer -> Integer -> (SystemState, StdGen)
-- simulation 0 nclients = (initialState nclients, randomGen)
-- simulation niterations nclients =  simulationStep (simulation (niterations-1) nclients)

-- bufferToList :: Buffer -> [SystemEvent]
-- bufferToList buf = fst (randomEventPick buf randomGen)
--     where {
--         randomEventPick b gen
--          | null (concat (Map.elems b)) = ([], gen)
--          | otherwise = let
--                             ch = getRandomElementOfList [ k | (k,v)<- Map.toList b, not (null v)] gen
--                             m:ms = Map.findWithDefault [] ch b
--                             newGen = snd (randomR (0::Int, 11) gen)
--                             tmpResult = randomEventPick (Map.insert ch ms b) newGen
--                         in Data.Bifunctor.first (m :) tmpResult
--     }

-- generateSequenceOfMessages :: Integer -> Integer -> [SystemEvent]
-- generateSequenceOfMessages a b = (bufferToList . fst3 . fst)  (simulation a b) where fst3 (b,_,_) = b

-- shortTrace :: [ChatWorld]
-- shortTrace = initScanSAS processEvent (initialChatState 3) (generateSequenceOfMessages 20 3)

-- longTrace :: [ChatWorld]
-- longTrace = initScanSAS processEvent (initialChatState 4) (generateSequenceOfMessages 100 4)

-- generateTraceGivenMessages :: [SystemEvent] -> [ChatWorld]
-- generateTraceGivenMessages = initScanSAS processEvent (initialChatState 3)

-- evalChatProp :: ChatWorld -> Atom ChatVal -> Either Error Prop
-- evalChatProp (SAS _ e s2) (Atom propName vals) =
--         case (propName, vals) of
--             ("IsMember", [ChatValClient cid, ChatValChannel chid]) -> if chid `elem` Map.findWithDefault [] cid (fst s2) then Right PropTrue else Right PropFalse
--             ("IsSameClient", [ChatValClient cid1, ChatValClient cid2]) -> if cid1 == cid2 then Right PropTrue else Right PropFalse
--             ("Left", [ChatValAction aid, ChatValClient cid, ChatValChannel chid]) -> if e == Left (Leave aid cid chid) then Right PropTrue else Right PropFalse
--             ("Joined", [ChatValAction aid, ChatValClient cid, ChatValChannel chid]) -> if e == Left (Join aid cid chid) then Right PropTrue else Right PropFalse
--             ("Sent", [ChatValAction aid, ChatValClient cid, ChatValChannel chid]) -> case e of
--                                                                                         Left (Send a c _ ch) -> if a==aid && c==cid && ch==chid then Right PropTrue  else Right PropFalse
--                                                                                         _ -> Right PropFalse
--             ("ListRequested", [ChatValAction aid, ChatValClient cid]) -> if e == Left (List aid cid) then Right PropTrue else Right PropFalse
--             ("Shared", [ChatValAction aid, ChatValClient sid, ChatValClient rid]) -> case e of
--                                                                                         Right (Share a s _ r) -> if a==aid && s==sid && r==rid then Right PropTrue else Right PropFalse
--                                                                                         _ -> Right PropFalse
--             ("NewJoinNote", [ChatValAction aid, ChatValClient cid1, ChatValChannel chid, ChatValClient cid2]) -> if e == Right (NewJoin aid cid1 chid cid2) then Right PropTrue else Right PropFalse
--             ("NewLeaveNote", [ChatValAction aid, ChatValClient cid1, ChatValChannel chid, ChatValClient cid2]) -> if e == Right (NewLeave aid cid1 chid cid2) then Right PropTrue else Right PropFalse
--             ("ChannelListNote", [ChatValAction aid, ChatValClient cid, ChatValChannel chid]) -> if e == Right (ChannelList aid cid chid) then Right PropTrue else Right PropFalse
--             _ -> Left ("Could not eval " <> propName <> " on " <> show vals)

-- instance Bridge Error ChatVal ChatWorld where
--     bridgeEvalProp = evalChatProp

--     -- TODO quantify actions with the action component of the SAS
--     bridgeQuantify (SAS _ m s2) tyname =
--         case tyname of
--             "Client" -> Right (map ChatValClient (Map.keys (fst s2)))
--             "Channel" -> Right (map ChatValChannel (Set.toList (Set.fromList (concat (Map.elems (fst s2))))))
--             "Action" -> Right (map ChatValAction [0..(snd s2)] )
--             _ -> Left ("Could not quantify over " <> tyname)

-- instance TruncBridge Error ChatVal ChatWorld where
--     truncBridgeEmpty _ = Set.fromList ["Client", "Channel", "Action"]
--     truncBridgeOracle w p =
--       let prop = evalChatProp w p in
--       case prop of
--           Left e -> Left e
--           Right q ->
--             case q of
--               PropTrue -> Right TriBoolTrue
--               PropFalse -> Right TriBoolFalse
--               _ -> Right TriBoolUnknown
