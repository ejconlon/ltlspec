module Ltlspec.Models.DinningHakker where

import Data.Map as M

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

class Msg m

instance Msg HakkerMsg
instance Msg ChopstickMsg

data Hakker = Hakker
  { hid :: HakkerId
  -- in-transit messages sent by Hakker
  , hkMsgs :: [HakkerMsg]
  , hkState :: HakkerState
  , lchop :: ChopstickId
  , rchop :: ChopstickId
  } deriving stock (Eq, Show)

data Chopstick = Chopstick
  { cid :: ChopstickId
  -- in-transit messages sent by Chopstick
  , chopMsgs :: [ChopstickMsg]
  } deriving stock (Eq, Show)

type Hakkers = M.Map HakkerId Hakker
type Chopsticks = M.Map ChopstickId Chopstick
type Traces = [Trace]

data GlobalState = GlobalState Hakkers Chopsticks
  deriving stock (Eq, Show)
data Trace = Trace TimeStamp (Either HakkerMsg ChopstickMsg) GlobalState
  deriving stock (Eq, Show)

-- Is this needed?
class Actor a where
    send :: Actor to => Msg msg => a -> to -> msg -> GlobalState
    recv :: Actor from => Msg msg => from -> a -> msg -> GlobalState

instance Actor Hakker where
    send to from msg = undefined
    recv to from msg = undefined

instance Actor Chopstick where
    send to from msg = undefined
    recv to from msg = undefined

-- data World = ?
