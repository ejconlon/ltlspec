module Ltlspec.System.TBarrier
  ( TBarrier
  , newTBarrier
  , newTBarrierIO
  , signalTBarrier
  , waitingTBarrier
  , awaitTBarrier
  ) where

import Control.Concurrent.STM (STM, retry)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVar, newTVarIO, readTVar)
import Control.Monad (when)

-- | A simple barrier that allows you to wait for N signal operations.
-- Use 'TSem' for this purpose unless you need non-blocking (observable) semantics.
newtype TBarrier = TBarrier { unTBarrier :: TVar Int }

-- | Create a new barrier (in STM)
-- The argument is the expected number of signals.
newTBarrier :: Int -> STM TBarrier
newTBarrier = fmap TBarrier . newTVar

-- | Create a new barrier (in IO)
newTBarrierIO :: Int -> IO TBarrier
newTBarrierIO = fmap TBarrier . newTVarIO

-- | Signal the barrier. Be careful not to do this unless you're expected to!
signalTBarrier :: TBarrier -> STM ()
signalTBarrier tb = modifyTVar' (unTBarrier tb) (\i -> i - 1)

-- | Is the barrier still waiting for any signals?
waitingTBarrier :: TBarrier -> STM Bool
waitingTBarrier = fmap (> 0) . readTVar . unTBarrier

-- | Wait for all signals to release the barrier.
awaitTBarrier :: TBarrier -> STM ()
awaitTBarrier tb = do
  waiting <- waitingTBarrier tb
  when waiting retry
