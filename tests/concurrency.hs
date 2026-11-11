module Main where

import Control.Concurrent (getNumCapabilities, threadDelay, yield)
import Control.Concurrent.MVar
import Control.Concurrent.QSem
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (forM_, replicateM_, when)
import qualified Data.Map as Map
import GHC.Clock (getMonotonicTimeNSec)

-- A queue of work as AIHC's task graph keeps it: take the next ready item,
-- or Nothing when all is done, or wait when some is still to come.
takeReady :: TVar ([Int], Int) -> STM (Maybe Int)
takeReady var = do
  (ready, remaining) <- readTVar var
  case ready of
    next : rest -> writeTVar var (rest, remaining) >> pure (Just next)
    []
      | remaining == 0 -> pure Nothing
      | otherwise -> retry

worker :: TVar ([Int], Int) -> TVar [Int] -> IO ()
worker queue done = do
  next <- atomically (takeReady queue)
  case next of
    Nothing -> pure ()
    Just n -> do
      atomically $ do
        modifyTVar' done (n :)
        -- Finishing an item below 3 makes another one ready.
        modifyTVar' queue (\(ready, remaining) -> (if n < 3 then ready ++ [n + 10] else ready, remaining - 1))
      worker queue done

main :: IO ()
main = do
  -- MVar
  var <- newMVar (1 :: Int)
  print =<< readMVar var
  print =<< takeMVar var
  print =<< isEmptyMVar var
  print =<< tryTakeMVar var
  print =<< tryReadMVar var
  putMVar var 2
  print =<< tryPutMVar var 3
  print =<< swapMVar var 4
  modifyMVar_ var (pure . (* 10))
  print =<< modifyMVar var (\n -> pure (n + 1, show n))
  print =<< withMVar var (pure . (+ 100))
  print =<< readMVar var
  failed <- try (modifyMVar_ var (\_ -> throwIO (ErrorCall "inside")))
  print (failed :: Either ErrorCall ())
  print =<< readMVar var
  empty <- newEmptyMVar :: IO (MVar Int)
  print =<< tryReadMVar empty
  blocked <- try (takeMVar empty)
  print (blocked :: Either BlockedIndefinitelyOnMVar Int)

  -- QSem
  places <- newQSem 2
  waitQSem places
  waitQSem places
  signalQSem places
  bracket_ (waitQSem places) (signalQSem places) (putStrLn "inside the semaphore")
  forM_ [1 .. 3 :: Int] (\n -> bracket_ (waitQSem places) (signalQSem places) (print n))

  -- TVar
  counter <- newTVarIO (0 :: Int)
  replicateM_ 5 (atomically (modifyTVar' counter (+ 2)))
  print =<< readTVarIO counter
  print =<< atomically (stateTVar counter (\n -> (show n, n * 2)))
  print =<< atomically (swapTVar counter 7)
  print =<< atomically ((,) <$> readTVar counter <*> (writeTVar counter 8 >> readTVar counter))
  table <- atomically (newTVar (Map.empty :: Map.Map String Int))
  forM_ (words "to be or not to be") (\w -> atomically (modifyTVar table (Map.insertWith (+) w 1)))
  print . Map.toList =<< readTVarIO table

  -- TMVar
  slot <- newEmptyTMVarIO :: IO (TMVar String)
  print =<< atomically (isEmptyTMVar slot)
  print =<< atomically (tryReadTMVar slot)
  atomically (putTMVar slot "filled")
  print =<< atomically (readTMVar slot)
  print =<< atomically (tryPutTMVar slot "again")
  print =<< atomically (takeTMVar slot)
  print =<< atomically (tryTakeTMVar slot)
  full <- atomically (newTMVar 'x')
  print =<< atomically (swapTMVar full 'y')
  print =<< atomically (readTMVar full)
  viaStm <- atomically (newEmptyTMVar >>= \t -> putTMVar t (1 :: Int) >> pure t)
  print =<< atomically (takeTMVar viaStm)

  -- retry, orElse, check
  print =<< atomically ((takeTMVar slot >> pure "first") `orElse` pure "second")
  print =<< atomically ((check False >> pure 'a') `orElse` (check True >> pure 'b'))
  stuck <- try (atomically (readTMVar slot))
  print (stuck :: Either BlockedIndefinitelyOnSTM String)
  thrown <- try (atomically (throwSTM (ErrorCall "from stm")))
  print (thrown :: Either ErrorCall ())

  -- The task graph
  queue <- newTVarIO ([1, 2, 3, 4], 6)
  done <- newTVarIO []
  worker queue done
  print . reverse =<< readTVarIO done
  print =<< readTVarIO queue

  -- Capabilities and clocks
  capabilities <- getNumCapabilities
  print (capabilities >= 1)
  before <- getMonotonicTimeNSec
  threadDelay 20000
  yield
  after <- getMonotonicTimeNSec
  when (after - before < 20000000) (putStrLn "the delay was too short")
  print (after > before)
