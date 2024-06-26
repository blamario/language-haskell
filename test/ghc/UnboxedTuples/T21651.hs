{-# LANGUAGE MagicHash, ScopedTypeVariables, UnboxedTuples #-}

-- This test is adapted from setnumcapabilities001.

import GHC.Conc hiding (threadWaitRead, threadWaitWrite)
import GHC.Exts
import GHC.IO.Encoding
import System.Environment
import System.IO
import Control.Monad
import Text.Printf
import Data.Time.Clock
import Control.DeepSeq

import System.Posix.IO
import System.Posix.Types
import Control.Concurrent
import Control.Exception

passTheParcel :: Int -> IO (IO ())
passTheParcel n = do
  pipes@(p1 : rest) <- forM [0..n-1] $ \_ -> createPipe
  rs@((_,tid1) : _) <- forM (pipes `zip` (rest ++ [p1])) $ \((readfd, _), (_, writefd)) -> do
    let
      read = fdRead readfd $ fromIntegral 1
      write = fdWrite writefd
    mv <- newEmptyMVar
    tid <- forkIO $ let
      loop = flip catch (\(x :: IOException) -> pure ()) $ forever $ do
        threadWaitRead readfd
        (s, _) <- read
        threadWaitWrite writefd
        write s
      cleanup = do
        closeFdWith closeFd readfd
        closeFdWith closeFd writefd
        putMVar mv ()
      in loop `finally` cleanup
    pure (mv, tid)

  let
    cleanup = do
      killThread tid1
      forM_ rs $ \(mv, _) -> takeMVar mv

  fdWrite (snd p1) "a"
  pure cleanup


main = do
  setLocaleEncoding latin1 -- fdRead and fdWrite depend on the current locale
  [n,q,t,z] <- fmap (fmap read) getArgs
  cleanup_ptp <- passTheParcel z
  t <- forkIO $ do
    forM_ (cycle ([n,n-1..1] ++ [2..n-1])) $ \m -> do
      setNumCapabilities m
      threadDelay t
  printf "%d\n" (nqueens q)
  cleanup_ptp
  killThread t
      -- If we don't kill the child thread, it might be about to
      -- call setNumCapabilities() in C when the main thread exits,
      -- and chaos can ensue.  See #12038

nqueens :: Int -> Int
nqueens nq = length (pargen 0 [])
 where
    safe :: Int -> Int -> [Int] -> Bool
    safe x d []    = True
    safe x d (q:l) = x /= q && x /= q+d && x /= q-d && safe x (d+1) l

    gen :: [[Int]] -> [[Int]]
    gen bs = [ (q:b) | b <- bs, q <- [1..nq], safe q 1 b ]

    pargen :: Int -> [Int] -> [[Int]]
    pargen n b
       | n >= threshold = iterate gen [b] !! (nq - n)
       | otherwise      = concat bs
       where bs = map (pargen (n+1)) (gen [b]) `using` parList rdeepseq

    threshold = 3

using :: a -> Strategy a -> a
x `using` strat = runEval (strat x)

type Strategy a = a -> Eval a

newtype Eval a = Eval (State# RealWorld -> (# State# RealWorld, a #))

runEval :: Eval a -> a
runEval (Eval x) = case x realWorld# of (# _, a #) -> a

instance Functor Eval where
  fmap = liftM

instance Applicative Eval where
  pure x = Eval $ \s -> (# s, x #)
  (<*>)  = ap

instance Monad Eval where
  return = pure
  Eval x >>= k = Eval $ \s -> case x s of
                                (# s', a #) -> case k a of
                                                      Eval f -> f s'

parList :: Strategy a -> Strategy [a]
parList strat = traverse (rparWith strat)

rpar :: Strategy a
rpar  x = Eval $ \s -> spark# x s

rseq :: Strategy a
rseq x = Eval $ \s -> seq# x s

rparWith :: Strategy a -> Strategy a
rparWith s a = do l <- rpar r; return (case l of Lift x -> x)
  where r = case s a of
              Eval f -> case f realWorld# of
                          (# _, a' #) -> Lift a'

data Lift a = Lift a

rdeepseq :: NFData a => Strategy a
rdeepseq x = do rseq (rnf x); return x
