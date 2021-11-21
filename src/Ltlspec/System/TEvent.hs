module Ltlspec.System.TEvent
  ( TEvent
  , newTEvent
  , newTEventIO
  , newTEventDelay
  , isSetTEvent
  , setTEvent
  , awaitTEvent
  ) where

import Control.Concurrent.STM (STM, retry)
import Control.Concurrent.STM.TVar (TVar)
import qualified Control.Concurrent.STM.TVar as TVar
import Control.DeepSeq (NFData (..))
import Ltlspec.System.Time (TimeDelta, timeDeltaToMicros)

-- | An variable that starts unset (as False) and can be set once (to True).
-- This is useful for recording one-way transitions like /open/ to /closed/.
newtype TEvent = TEvent { unTEvent :: TVar Bool }
  deriving stock (Eq)

instance NFData TEvent where
  rnf (TEvent v) = seq v ()

-- | Create a new 'TEvent' (in STM).
newTEvent :: STM TEvent
newTEvent = fmap TEvent (TVar.newTVar False)

-- | Create a new 'TEvent' (in IO).
newTEventIO :: IO TEvent
newTEventIO = fmap TEvent (TVar.newTVarIO False)

-- | Set the event after a delay in us (similar to 'threadDelay')
newTEventDelay :: TimeDelta -> IO TEvent
newTEventDelay = fmap TEvent . TVar.registerDelay . timeDeltaToMicros

-- | Return true if the event has been set.
-- Once it has been set, it remains so.
isSetTEvent :: TEvent -> STM Bool
isSetTEvent = TVar.readTVar . unTEvent

-- | Set the event. Idempotent; additional calls have no observable effect.
setTEvent :: TEvent -> STM ()
setTEvent = flip TVar.writeTVar True . unTEvent

-- | Wait for the TEvent to be set.
awaitTEvent :: TEvent -> STM ()
awaitTEvent (TEvent t) = do
  v <- TVar.readTVar t
  if v
    then pure ()
    else retry
