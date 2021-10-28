module Ltlspec.Models.DinningHakker where

import qualified Data.Map as M

type TimeStamp = Int

type HakkerId = String

type ChopstickId = Int

data HakkerState =
      Thinking
    | Hungry
    | Eating

data ChopstickState =
      Free
    | Taken

data HakkerMsg =
      Take HakkerId ChopstickId
    | Put HakkerId ChopstickId

data ChopstickMsg =
      Grant ChopstickId HakkerId
    | Busy ChopstickId HakkerId

class Message m

instance Message HakkerMsg

instance Message ChopstickMsg

data Hakker = Hakker
                { hid :: HakkerId
                -- in-transit messages sent by Hakker
                -- to simulate network delay
                -- NOTE: not used for now
                , hkSends :: [HakkerMsg]
                -- Hakker's message queue
                , hkRecvs :: [ChopstickMsg]
                , hkState :: HakkerState
                , lchop :: ChopstickId
                , rchop :: ChopstickId
                , lhold :: Bool
                , rhold :: Bool
                }

defaultHakker :: Hakker
defaultHakker = Hakker
                  { hid = "DEFAULT"
                  , hkSends = []
                  , hkRecvs = []
                  , hkState = Thinking
                  , lchop = -1
                  , rchop = -1
                  , lhold = False
                  , rhold = False
                  }

data Chopstick = Chopstick
                   { cid :: ChopstickId
                   -- in-transit messages sent by Chopstick
                   -- to simulate network delay
                   -- NOTE: not used for now
                   , chopSends :: [ChopstickMsg]
                   -- Chopstick's message queue
                   , chopRecvs :: [HakkerMsg]
                   , chopState :: ChopstickState
                   }

defaultChopstick :: Chopstick
defaultChopstick = Chopstick
                     { cid = -1
                     , chopSends = []
                     , chopRecvs = []
                     , chopState = Free
                     }

type Hakkers = M.Map HakkerId Hakker

type Chopsticks = M.Map ChopstickId Chopstick

data GlobalState = GlobalState Hakkers Chopsticks

data World = World TimeStamp [Either HakkerMsg ChopstickMsg] GlobalState

type Trace = [World]

class Actor a where

instance Actor Hakker where

instance Actor Chopstick where

initState :: [HakkerId] -> GlobalState
initState hs = GlobalState hakkers chopsticks
    where
        l = length hs
        idx = init $ scanl (\c _ -> c + 1) 0 hs
        chopsticks = foldl (\m i -> M.insert i defaultChopstick{cid=i} m) M.empty idx
        hakkers = foldl (\m (i, h) -> M.insert h defaultHakker{hid=h, lchop=i, rchop=(i-1) `mod` l} m) M.empty (zip idx hs)

data Action =
      -- If the Hakker is Eating, send Put messages to both sides, and transfer to Thinking
      -- Otherwise do nothing.
      -- NOTE: if later we want to simulate delayed network, Hakker should also wait for confirm message from chopsticks
      HakkerThink HakkerId
      -- If Hakker is Thinking, send Take messages to both sides, and transfer to Hungry
      -- Otherwise do nothing
    | HakkerHungry HakkerId
      -- If Hakker is Hungry, check messages received from both sides.
      -- If haven't received messages from both side, stay Hungry
      -- If any message is Busy, stay Hungry
      -- If both sides of the messages are Grant, start Eating
      -- If Hakker is in other states, do nothing
      -- NOTE: A different implementation would be transfer back to Thinking is any message is Busy
    | HakkerEat HakkerId
      -- Deliver a message from chopRecvs, and respond accordingly
    | ChopstickResp ChopstickId

chopReceive :: Chopstick -> HakkerMsg -> Chopstick
chopReceive chop@Chopstick{chopRecvs=recvs} msg = chop {chopRecvs = msg : recvs}

hakkerReceive :: Hakker -> ChopstickMsg -> Hakker
hakkerReceive hakker@Hakker{hkRecvs=recvs} msg = hakker {hkRecvs = msg : recvs}

-- One system step in perfect network condition
-- This step function will cause deadlock for DinningHakker problem
-- If the Chopstick is Taken, and gets a Take message from the other Hakker,
-- the Chopstick will not process the Hakker's Take message.
-- NOTE: i.e., the Busy message is not used in this simulation function
stepPerfect :: Action -> World -> World
stepPerfect (HakkerThink h) world@(World ts ms gs@(GlobalState hs cs)) =
    let hk = hs M.! h in
    case hkState hk of
        Eating -> let
            leftCid = lchop hk
            rightCid = rchop hk
            leftMsg = Put h leftCid
            rightMsg = Put h rightCid
            leftChop' = chopReceive (cs M.! leftCid) leftMsg
            rightChop' = chopReceive (cs M.! rightCid) rightMsg
            cs' = M.insert leftCid leftChop' cs
            cs'' = M.insert rightCid rightChop' cs'
            hk' = hk {hkState=Thinking}
            hs' = M.insert h hk' hs
            gs' = GlobalState hs' cs''
            in
            World (ts+1) (Left rightMsg : Left leftMsg : ms) gs'
        _ -> World (ts+1) ms gs
stepPerfect (HakkerHungry h) world@(World ts ms gs@(GlobalState hs cs)) =
    let hk = hs M.! h in
    case hkState hk of
        Thinking -> let
            leftCid = lchop hk
            rightCid = rchop hk
            leftMsg = Take h leftCid
            rightMsg = Take h rightCid
            leftChop' = chopReceive (cs M.! leftCid) leftMsg
            rightChop' = chopReceive (cs M.! rightCid) rightMsg
            cs' = M.insert leftCid leftChop' cs
            cs'' = M.insert rightCid rightChop' cs'
            hk' = hk {hkState=Hungry}
            hs' = M.insert h hk' hs
            gs' = GlobalState hs' cs''
            in
            World (ts+1) (Left rightMsg : Left leftMsg : ms) gs'
        _ -> World (ts+1) ms gs
stepPerfect (HakkerEat h) world@(World ts ms gs@(GlobalState hs cs)) =
    let hk = hs M.! h in
    case hkState hk of
        Hungry -> case hkRecvs hk of
            Grant _ _ : Grant _ _ : msgs -> let
                hk' = hk {hkRecvs=msgs, hkState=Eating}
                hs' = M.insert h hk' hs
                in
                World (ts+1) ms (GlobalState hs' cs)
            _ -> World (ts+1) ms gs
        _ -> World (ts+1) ms gs
stepPerfect (ChopstickResp c) world@(World ts ms gs@(GlobalState hs cs)) =
    let chop = cs M.! c in
    case chopState chop of
        Free -> case chopRecvs chop of
            Take hid _ : msgs -> let
                hk = hs M.! hid
                msg = (Grant c hid)
                hk' = hakkerReceive hk msg
                hs' = M.insert hid hk' hs
                cs' = M.insert c (chop {chopRecvs=msgs}) cs
                in World (ts+1) (Right msg : ms) (GlobalState hs' cs')
            _ -> World (ts+1) ms gs
        Taken -> case chopRecvs chop of
            Put _ _ : _ -> undefined
            _ -> World (ts+1) ms gs

-- Atomic Propositions
