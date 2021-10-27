module Ltlspec.Models.DinningHakker where

import Data.Map as M

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

data Hakker = Hakker
                { hid :: HakkerId
                , hkMsgs :: [HakkerMsg]
                , hkState :: HakkerState
                , lchop :: Int
                , rchop :: Int
                }

data Chopstick = Chopstick
                   { cid :: ChopstickId
                   , chopMsgs :: [ChopstickMsg]
                   }

type Hakkers = M.Map String Hakker
type Chopsticks = M.Map Int Chopstick
type Traces = [Trace]

data GlobalState = GlobalState Hakkers Chopsticks
data Trace = Trace TimeStamp (Either HakkerMsg ChopstickMsg) GlobalState

-- Is this needed?
class Actor a where
    send :: a -> a
    recv :: a -> a

-- data World = ?

