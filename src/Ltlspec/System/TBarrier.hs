module Ltlspec.System.TBarrier
  ( TBarrier
  , newTBarrier
  , newTBarrierIO
  , signalTBarrier
  , waitingTBarrier
  ) where

import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVar, newTVarIO, readTVar)

newtype TBarrier = TBarrier { unTBarrier :: TVar Int }

newTBarrier :: Int -> STM TBarrier
newTBarrier = fmap TBarrier . newTVar

newTBarrierIO :: Int -> IO TBarrier
newTBarrierIO = fmap TBarrier . newTVarIO

signalTBarrier :: TBarrier -> STM ()
signalTBarrier tb = modifyTVar' (unTBarrier tb) (\i -> i - 1)

waitingTBarrier :: TBarrier -> STM Bool
waitingTBarrier = fmap (> 0) . readTVar . unTBarrier
