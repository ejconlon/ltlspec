module Ltlspec.Models.Ping where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Ltlspec

pingTheory :: Theory
pingTheory = Theory
  { theoryTypes = ["ClientId", "MessageId"]
  , theoryProps = Map.fromList
      [ ("IsMessage", ["ClientId", "MessageId"])
      , ("IsResponse", ["MessageId", "MessageId"])
      ]
  , theoryAxioms = Map.fromList []
  }

type PingId = Int

type PingData = Set PingId

data PingWorld = PingWorld
  { pwDataA :: !PingData
  , pwDataB :: !PingData
  }

