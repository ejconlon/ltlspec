module Ltlspec.Models.DinningHakker where

import qualified Data.Map.Strict as M
import Data.Sequence as S (Seq (..), empty)
import Ltlspec (
    Theory (..)
  , Prop (..)
  , Binder(..)
  , propAtom
  , propAlways
  , propEventually
  , propIf
  , propForAllNested)

type TimeStamp = Int

type HakkerId = String

type ChopstickId = Int

data HakkerState =
    Thinking
  | Hungry
  | Eating
  deriving stock (Eq, Show)

data ChopstickState =
    Free
  | Taken
  deriving stock (Eq, Show)

data HakkerMsg =
    Take HakkerId ChopstickId
  | Put HakkerId ChopstickId
  deriving stock (Eq, Show)

data ChopstickMsg =
    Grant ChopstickId HakkerId
  | Busy ChopstickId HakkerId
  deriving stock (Eq, Show)

class Message m

instance Message HakkerMsg

instance Message ChopstickMsg

data Hakker = Hakker
                { hid :: HakkerId
                -- in-transit messages sent by Hakker
                -- to simulate network delay
                -- NOTE: not used for now
                , hkSends :: Seq HakkerMsg
                -- Hakker's message queue
                , hkRecvs :: Seq ChopstickMsg
                , hkState :: HakkerState
                , lchop :: ChopstickId
                , rchop :: ChopstickId
                , lhold :: Bool
                , rhold :: Bool
                } deriving stock (Eq, Show)

defaultHakker :: Hakker
defaultHakker = Hakker
                  { hid = "DEFAULT"
                  , hkSends = S.empty
                  , hkRecvs = S.empty
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
                   , chopSends :: S.Seq ChopstickMsg
                   -- Chopstick's message queue
                   , chopRecvs :: S.Seq HakkerMsg
                   , chopState :: ChopstickState
                   } deriving stock (Eq, Show)

defaultChopstick :: Chopstick
defaultChopstick = Chopstick
                     { cid = -1
                     , chopSends = S.empty
                     , chopRecvs = S.empty
                     , chopState = Free
                     }

type Hakkers = M.Map HakkerId Hakker

type Chopsticks = M.Map ChopstickId Chopstick

data GlobalState = GlobalState Hakkers Chopsticks deriving stock (Eq, Show)

data World = World TimeStamp [Either HakkerMsg ChopstickMsg] GlobalState deriving stock (Eq, Show)

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

initWorld :: GlobalState -> World
initWorld gs = World 0 [] gs

data Action =
    -- NOTE: if later we want to simulate delayed network, Hakker should also wait for confirm message from chopsticks
    HakkerThink HakkerId
  | HakkerHungry HakkerId
    -- NOTE: A different implementation would be transfer back to Thinking is any message is Busy
  | HakkerEat HakkerId
  | ChopstickResp ChopstickId
  deriving stock (Eq, Show)

chopReceive :: Chopstick -> HakkerMsg -> Chopstick
chopReceive chop@Chopstick{chopRecvs=recvs} msg = chop {chopRecvs = msg :<| recvs}

hakkerReceive :: Hakker -> ChopstickMsg -> Hakker
hakkerReceive hakker@Hakker{hkRecvs=recvs} msg = hakker {hkRecvs = msg :<| recvs}

tick :: World -> World
tick (World ts ms gs)= World (ts + 1) ms gs

-- One system step in perfect network condition
-- This step function will cause deadlock for DinningHakker problem
-- If the Chopstick is Taken, and gets a Take message from the other Hakker,
-- the Chopstick will not process the Hakker's Take message.
-- NOTE: i.e., the Busy message is not used in this simulation function
stepPerfect :: Action -> World -> World
-- If the Hakker is Eating, send Put messages to both sides, and transfer to Thinking
-- Otherwise do nothing.
stepPerfect (HakkerThink h) w@(World ts ms (GlobalState hs cs)) =
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
      _ -> tick w
-- If Hakker is Thinking, send Take messages to both sides, and transfer to Hungry
-- Otherwise do nothing
stepPerfect (HakkerHungry h) w@(World ts ms (GlobalState hs cs)) =
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
    _ -> tick w
-- If Hakker is Hungry, check messages received from both sides.
-- If haven't received messages from both side, stay Hungry
-- If any message is Busy, stay Hungry
-- If both sides of the messages are Grant, start Eating
-- If Hakker is in other states, do nothing
stepPerfect (HakkerEat h) w@(World ts ms (GlobalState hs cs)) =
  let hk = hs M.! h in
  case hkState hk of
    Hungry -> case hkRecvs hk of
      msgs :|> Grant _ _ :|> Grant _ _  -> let
        hk' = hk {hkRecvs=msgs, hkState=Eating}
        hs' = M.insert h hk' hs
        in
        World (ts+1) ms (GlobalState hs' cs)
      _ -> tick w
    _ -> tick w
-- Deliver a message from chopRecvs, and respond accordingly
stepPerfect (ChopstickResp c) w@(World ts ms (GlobalState hs cs)) =
  let chop = cs M.! c in
  case chopState chop of
    Free -> case chopRecvs chop of
      msgs :|> Take hid _ -> let
        hk = hs M.! hid
        msg = (Grant c hid)
        hk' = hakkerReceive hk msg
        hs' = M.insert hid hk' hs
        cs' = M.insert c (chop {chopRecvs=msgs, chopState=Taken}) cs
        in World (ts+1) (Right msg : ms) (GlobalState hs' cs')
      _ -> tick w
    Taken -> case chopRecvs chop of
      msgs :|> Put _ _ -> let
        cs' = M.insert c (chop {chopRecvs=msgs, chopState=Free}) cs
        in
        World (ts+1) ms (GlobalState hs cs')
      msgs :|> msg@(Take _ _) -> let
        cs' = M.insert c (chop {chopRecvs=msg :<| msgs}) cs
        in
        World (ts+1) ms (GlobalState hs cs')
      _ -> tick w

genTrace :: [Action] -> World -> [World]
genTrace [] w = [w]
genTrace (a : as) w =
  let nextw = stepPerfect a w
  in w : genTrace as nextw

scheduler :: [HakkerId] -> [Action]
scheduler = undefined

dhworld3 :: World
dhworld3 = initWorld $ initState ["Ghosh", "Boner", "Klang"]

dhaction3 :: [Action]
dhaction3 = [
  HakkerHungry "Ghosh",
  HakkerHungry "Boner",
  HakkerHungry "Klang",
  ChopstickResp 0,
  ChopstickResp 1,
  ChopstickResp 2,
  HakkerEat "Ghosh"
  ]

dhtrace3 :: [World]
dhtrace3 = genTrace dhaction3 dhworld3

-- Domain Theory
dinningHakkerTheory :: Theory
dinningHakkerTheory = Theory
                        { theoryTypes = ["HakkerId", "ChopstickId", "TimeStamp", "HakkerMsg", "ChopstickMsg"]
                        , theoryProps = M.fromList [
                            ("isThinking",["HakkerId"])
                          , ("isHungry",["HakkerId"])
                          , ("isEating",["HakkerId"])
                          -- A message that has been received but not yet delivered by a chopstick
                          -- i.e., the message is currently in the chopRecvs Seq
                          , ("receivedNotDelivered", ["ChopstickId, HakkerMsg"])
                          , ("fromAdjacent", ["ChopstickId, HakkerMsg"])
                        ]
                        , theoryAxioms = M.fromList [
                          -- checking liveness properties for all hakkers
                          -- All hakkers will start from thinking, and should eventually start eating
                          -- Because the Eating state is mutually exclusive for adjacent Hakkers
                          -- forall h: Hakker. isThinking(h) -> F[isEating(h)]
                          ("Liveness",
                            propAlways
                              (PropForAll
                                (Binder "h" "HakkerId")
                                (propIf
                                  (propAtom "isThinking" ["h"])
                                  (propEventually
                                    (propAtom "isEating" ["h"]))
                                )
                              )
                          ),
                          ("ReceiveFromAdjacentHakkers",
                            propAlways
                              (propForAllNested [("c", "ChopstickId"), ("hm", "HakkerMsg")]
                                (propIf
                                  (propAtom "receivedNotDeliverd" ["c", "hm"])
                                  (propAtom "fromAdjacent" ["c", "hm"])
                                )
                              )
                          )
                        ]
                        }
