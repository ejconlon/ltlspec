module Ltlspec.Models.Ping.Common where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

-- | We're sending pings and receiving pongs.
data PingMessage =
    PingMessagePing
  | PingMessagePong
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)
