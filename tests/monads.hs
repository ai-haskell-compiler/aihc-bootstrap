module Main where

import Control.Applicative (Alternative (..), Const (..), optional)
import Control.Exception (bracket, evaluate, finally, onException, throwIO, try, ErrorCall (..))
import Control.Monad (foldM, forM, forM_, forever, guard, replicateM, unless, when, zipWithM_, (>=>), join, void)
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, catchE, runExceptT, throwE)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Reader (ReaderT, ask, asks, local, runReaderT)
import Control.Monad.Trans.State.Strict (State, StateT, evalState, evalStateT, execState, get, gets, modify, modify', put, runState, runStateT, state)
import Data.Bifunctor (bimap, first)
import Data.Bits (complement, countLeadingZeros, countTrailingZeros, popCount, shiftL, shiftR, testBit, xor, (.&.), (.|.), setBit, clearBit, bit)
import Data.Either (isLeft, lefts, partitionEithers, rights)
import Data.Foldable (foldrM, for_, traverse_)
import Data.Function (on)
import Data.Functor (($>), (<&>))
import Data.Functor.Identity (Identity (..))
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int8)
import Data.List (sortBy)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid (All (..), Any (..), First (..), Sum (..))
import Data.Ord (Down (..), comparing)
import Data.STRef (modifySTRef', newSTRef, readSTRef)
import Data.Word (Word16, Word64, Word8)

-- A counter in State.
labels :: [String] -> State Int [(Int, String)]
labels = mapM (\name -> do
  n <- get
  put (n + 1)
  pure (n, name))

-- State over IO, with Reader above it.
walk :: ReaderT String (StateT Int IO) ()
walk = do
  prefix <- ask
  forM_ [1 .. 3 :: Int] $ \step -> do
    total <- lift get
    lift (lift (putStrLn (prefix ++ show step ++ " after " ++ show total)))
    lift (modify (+ step))
  inner <- local (++ "> ") (asks length)
  lift (modify' (* inner))

-- Errors that skip the rest, and can be caught.
safeDivide :: Int -> Int -> ExceptT String (State [String]) Int
safeDivide a b = do
  lift (modify (("dividing " ++ show a) :))
  when (b == 0) (throwE "division by zero")
  pure (a `div` b)

sumST :: [Int] -> Int
sumST numbers = runST (do
  total <- newSTRef 0
  mapM_ (\n -> modifySTRef' total (+ n)) numbers
  readSTRef total)

main :: IO ()
main = do
  print (runState (labels ["a", "b", "c"]) 10)
  print (evalState (gets (* 2)) 21, execState (mapM_ (\n -> modify (+ n)) [1 .. 10]) 0, runState (state (\s -> (s, s + 1))) 5)
  result <- runStateT (runReaderT walk "step ") 0
  print result
  print (runState (runExceptT (safeDivide 10 2 >>= safeDivide 100)) [])
  print (runState (runExceptT (safeDivide 10 0 >>= safeDivide 100)) [])
  print (evalState (runExceptT (safeDivide 1 0 `catchE` (\problem -> pure (length problem))) :: State [String] (Either () Int)) [])
  found <- runMaybeT (do
    a <- MaybeT (pure (Just 1))
    b <- MaybeT (pure Nothing) <|> MaybeT (pure (Just 2))
    lift (putStrLn "in MaybeT")
    pure (a + b :: Int))
  print (found, runIdentity (runMaybeT (MaybeT (Identity (Nothing :: Maybe Int)) >>= pure . (+ 1))))
  print (sumST [1 .. 100], Just 1 <|> Just 2, Nothing <|> Just 'x', [1, 2] <|> [3 :: Int], optional (Nothing :: Maybe Int), optional [1 :: Int])
  print (getConst (traverse (\x -> Const [x, x]) "ab"), getSum (foldMap Sum [1 .. 10 :: Int]), getAll (foldMap (All . even) [2, 4 :: Int]), getAny (foldMap (Any . odd) [2, 4 :: Int]))
  print (getFirst (First (Just 1) <> First Nothing <> First (Just (3 :: Int))), Sum 2 <> Sum (3 :: Int), sortBy (comparing Down) [3, 1, 2 :: Int])
  print (foldM (\total n -> if n > 0 then Just (total + n) else Nothing) 0 [1, 2, 3 :: Int], foldM (\_ n -> if n > 0 then Just n else Nothing) 0 [1, -2 :: Int])
  print (replicateM 2 "ab", join [[1], [2, 3 :: Int]], (pure . (+ 1) >=> pure . (* 2)) 5 :: Maybe Int, [x | x <- [1 .. 10 :: Int], then' x])
  print (Just 1 $> 'x', Just 1 <&> (+ 1), void (Just 'x'), (do guard True; pure 'y') :: Maybe Char, (guard False >> pure 'z') :: [Char])
  print (partitionEithers [Left 'a', Right 1, Left 'b', Right (2 :: Int)], lefts [Left 1, Right 'x' :: Either Int Char], isLeft (Left 1 :: Either Int Int))
  print (bimap length show ("abc", 1 :: Int), first (+ 1) (1 :: Int, 'x'), ((==) `on` fst) (1 :: Int, 'a') (1, 'b'))
  folded <- foldrM (\n total -> putStrLn ("visit " ++ show n) >> pure (total + n)) 0 [1, 2, 3 :: Int]
  for_ (Just folded) print
  traverse_ print (3 :| [1, 2 :: Int])
  let list = 5 :| [3, 8 :: Int]
  print (list, NE.toList list, NE.head list, NE.last list, NE.map (* 2) list, NE.nonEmpty ([] :: [Int]), NE.nonEmpty "ab", maximum list, Just (1 :| []) < Just (1 :| [2 :: Int]))
  -- Bits at several widths.
  print (0xF0 .&. 0x3C :: Int, 0xF0 .|. 0x0F :: Word8, xor 0xFF 0x0F :: Word8, complement 0 :: Word16, complement 5 :: Int8)
  print (shiftL 1 63 :: Word64, shiftL 1 7 :: Int8, shiftR (-16) 2 :: Int, shiftR 0x8000 3 :: Word16, shiftL 0xFF 4 :: Word8)
  print (popCount (0xFF00FF :: Int), popCount (-1 :: Int8), testBit (5 :: Int) 2, testBit (5 :: Int) 1, setBit (0 :: Word8) 7, clearBit (0xFF :: Word8) 0, bit 4 :: Int)
  print (countLeadingZeros (1 :: Word8), countTrailingZeros (8 :: Word16), countLeadingZeros (0 :: Int8), countTrailingZeros (0 :: Word64))
  -- Mutable cells.
  ref <- newIORef (0 :: Int)
  forM_ [1 .. 5] (\n -> modifyIORef' ref (+ n))
  old <- atomicModifyIORef' ref (\n -> (n * 2, n))
  now <- readIORef ref
  writeIORef ref 7
  final <- readIORef ref
  print (old, now, final)
  -- Cleanup runs whether or not the action fails.
  outcome <- try (bracket (putStrLn "acquire" >> pure 5) (\_ -> putStrLn "release") (\n -> putStrLn "use" >> throwIO (ErrorCall "failed") >> pure (n :: Int)))
  print (either (\(ErrorCall message) -> message) show outcome)
  (putStrLn "body" `finally` putStrLn "finally") `onException` putStrLn "not printed"
  forced <- try (evaluate (length [1, 2, 3 :: Int]))
  print (either (\(ErrorCall message) -> message) show forced)
  unless False (zipWithM_ (\n c -> putStrLn (show n ++ [c])) [1 :: Int ..] "ab")
  where
    then' x = x `mod` 4 == 0
