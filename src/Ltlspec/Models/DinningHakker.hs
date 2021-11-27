module Ltlspec.Models.DinningHakker where

import qualified Data.Map.Strict as M
import Data.Sequence as S (Seq (..), empty)
import Ltlspec (propAlways, propAtom, propEventually, propForAllNested, propIf)
import Ltlspec.Types (Binder (..), Commented (..), Prop (..), SAS (..), Theory (..))

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
    Take TimeStamp HakkerId ChopstickId
  | Put TimeStamp HakkerId ChopstickId
  deriving stock (Eq, Show)

data ChopstickMsg =
    Grant TimeStamp ChopstickId HakkerId
  | Busy TimeStamp ChopstickId HakkerId
  deriving stock (Eq, Show)

data Hakker = Hakker
  { hakkerId :: HakkerId
  -- Hakker's message queue, received but not delivered
  , hkRecvs :: Seq ChopstickMsg
  , hkState :: HakkerState
  , lchop :: ChopstickId
  , rchop :: ChopstickId
  , lhold :: Bool
  , rhold :: Bool
  } deriving stock (Eq, Show)

defaultHakker :: Hakker
defaultHakker = Hakker
  { hakkerId = "DEFAULT"
  , hkRecvs = S.empty
  , hkState = Thinking
  , lchop = -1
  , rchop = -1
  , lhold = False
  , rhold = False
  }

data Chopstick = Chopstick
  { chopId :: ChopstickId
  -- Chopstick's message queue
  , chopRecvs :: S.Seq HakkerMsg
  , chopState :: ChopstickState
  } deriving stock (Eq, Show)

defaultChopstick :: Chopstick
defaultChopstick = Chopstick
  { chopId = -1
  , chopRecvs = S.empty
  , chopState = Free
  }

type Hakkers = M.Map HakkerId Hakker

type Chopsticks = M.Map ChopstickId Chopstick

type Message = Either HakkerMsg ChopstickMsg

newtype Network = Network
  { packets :: [Message]
  }

sendMessage:: Message -> Network -> Network
sendMessage = error "TODO"

recvMessage:: Network -> Network
recvMessage = error "TODO"

data GlobalState = GlobalState
  { timestamp:: TimeStamp
  , hakkers:: Hakkers
  , chopsticks:: Chopsticks
  , messages:: [Message]
  }
  deriving stock (Eq, Show)

defaultGlobalState:: GlobalState
defaultGlobalState = GlobalState
  { timestamp = 0
  , hakkers = M.empty
  , chopsticks = M.empty
  , messages = []
  }

initState :: [HakkerId] -> GlobalState
initState hs = defaultGlobalState {hakkers = hks, chopsticks = chops}
  where
    l = length hs
    idx = init $ scanl (\c _ -> c + 1) 0 hs
    chops = foldl (\m i -> M.insert i defaultChopstick{chopId=i} m) M.empty idx
    hks = foldl (\m (i, h) -> M.insert h defaultHakker{hakkerId=h, lchop=i, rchop=(i-1) `mod` l} m) M.empty (zip idx hs)

tick :: GlobalState -> GlobalState
tick gs = gs {timestamp = timestamp gs + 1}

data Action =
    HakkerThink HakkerId
  | HakkerHungry HakkerId
  | HakkerEat HakkerId
  | ChopstickResp ChopstickId
  -- Below are messages for delayed network
  | Nop
  deriving stock (Eq, Show)

chopReceive :: HakkerMsg -> Chopstick -> Chopstick
chopReceive msg chop@Chopstick{chopRecvs=recvs} = chop {chopRecvs = msg :<| recvs}

hakkerReceive :: ChopstickMsg -> Hakker -> Hakker
hakkerReceive msg hakker@Hakker{hkRecvs=recvs} = hakker {hkRecvs = msg :<| recvs}

-- | One system step in perfect network condition
-- This step function will not cause deadlock for DinningHakker problem.
-- All messages are immediately received.
-- All messages are processed in the order they are received.
-- NOTE: the Busy message is not used in this simulation function
-- NOTE: A different implementation would be transfer back to Thinking is any message is Busy
-- NOTE: if later we want to simulate packet loss, Hakker should also wait for confirmation from chopsticks
stepPerfect :: Action -> GlobalState -> GlobalState
-- If the Hakker is Eating, send Put messages to both sides, and transfer to Thinking
-- Otherwise do nothing.
stepPerfect (HakkerThink h) gs@GlobalState{timestamp=ts, hakkers=hs, chopsticks=cs, messages=ms} =
  let hk = hs M.! h in
  case hkState hk of
    Eating -> let
      leftCid = lchop hk
      rightCid = rchop hk
      leftMsg = Put ts h leftCid
      rightMsg = Put ts h rightCid
      cs' = M.adjust (chopReceive leftMsg) leftCid cs
      cs'' = M.adjust (chopReceive rightMsg) rightCid cs'
      hk' = hk {hkState=Thinking}
      hs' = M.insert h hk' hs
      ms' = Left rightMsg : Left leftMsg :ms
      in
      tick gs {hakkers=hs', chopsticks=cs'', messages=ms'}
    _ -> tick gs
-- If Hakker is Thinking, send Take messages to both sides, and transfer to Hungry
-- Otherwise do nothing
stepPerfect (HakkerHungry h) gs@GlobalState{timestamp=ts, hakkers=hs, chopsticks=cs, messages=ms} =
  let hk = hs M.! h in
  case hkState hk of
    Thinking -> let
      leftCid = lchop hk
      rightCid = rchop hk
      leftMsg = Take ts h leftCid
      rightMsg = Take ts h rightCid
      cs' = M.adjust (chopReceive leftMsg) leftCid cs
      cs'' = M.adjust (chopReceive rightMsg) rightCid cs'
      hk' = hk {hkState=Hungry}
      hs' = M.insert h hk' hs
      ms' = Left rightMsg : Left leftMsg : ms
      in
      tick gs {hakkers=hs', chopsticks=cs'', messages=ms'}
    _ -> tick gs
-- If Hakker is Hungry, check messages received from both sides.
-- If haven't received messages from both side, stay Hungry
-- If any message is Busy, stay Hungry
-- If both sides of the messages are Grant, start Eating
-- If Hakker is in other states, do nothing
stepPerfect (HakkerEat h) gs@GlobalState{hakkers=hs, chopsticks=cs} =
  let hk = hs M.! h in
  case hkState hk of
    Hungry -> case hkRecvs hk of
      msgs :|> Grant {} :|> Grant {}  -> let
        hk' = hk {hkRecvs=msgs, hkState=Eating}
        hs' = M.insert h hk' hs
        in
        tick gs {hakkers=hs', chopsticks=cs}
      _ -> tick gs
    _ -> tick gs
-- Deliver a message from chopRecvs, and respond accordingly
stepPerfect (ChopstickResp c) gs@GlobalState{timestamp=ts, hakkers=hs, chopsticks=cs, messages=ms} =
  let chop = cs M.! c in
  case chopState chop of
    Free -> case chopRecvs chop of
      msgs :|> Take _ hid _ -> let
        msg = Grant ts c hid
        hs' = M.adjust (hakkerReceive msg) hid hs
        cs' = M.insert c (chop {chopRecvs=msgs, chopState=Taken}) cs
        in
        tick gs {hakkers=hs', chopsticks=cs', messages=Right msg : ms}
      _ -> tick gs
    Taken -> case chopRecvs chop of
      msgs :|> Put {} -> let
        cs' = M.insert c (chop {chopRecvs=msgs, chopState=Free}) cs
        in
        tick gs {hakkers=hs, chopsticks=cs'}
      -- If the message is already taken, but the next message is a Take.
      -- Move the Take message to the end of the list.
      -- This is fine, because there's only two Hakkers knowing the chopstick
      -- And only one of them will send Take message
      msgs :|> msg@Take {} -> let
        cs' = M.insert c (chop {chopRecvs=msg :<| msgs}) cs
        in
        tick gs {hakkers=hs, chopsticks=cs'}
      _ -> tick gs
-- Do not handle other cases
stepPerfect _ gs = gs

type World = SAS GlobalState Action

type Trace = [World]

genTrace :: [Action] -> GlobalState -> [World]
genTrace [] _ = []
genTrace (a : as) gs =
  let gs' = stepPerfect a gs
  in SAS gs a gs' : genTrace as gs'

scheduler :: [HakkerId] -> [Action]
scheduler = error "TODO"

dhinitState3 :: GlobalState
dhinitState3 = initState ["Ghosh", "Boner", "Klang"]

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
dhtrace3 = genTrace dhaction3 dhinitState3

-- Domain Theory
dinningHakkerTheory :: Theory
dinningHakkerTheory = Theory
  { theoryTypes = NoComment <$> ["HakkerId", "ChopstickId", "TimeStamp", "HakkerMsg", "ChopstickMsg"]
  , theoryProps = NoComment <$> M.fromList [
      ("isThinking",["HakkerId"])
    , ("isHungry",["HakkerId"])
    , ("isEating",["HakkerId"])
    -- A message that has been received but not yet delivered by a chopstick
    -- i.e., the message is currently in the chopRecvs Seq
    , ("receivedNotDelivered", ["ChopstickId, HakkerMsg"])
    , ("fromAdjacent", ["ChopstickId, HakkerMsg"])
  ]
  , theoryAxioms = NoComment <$> M.fromList [
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
