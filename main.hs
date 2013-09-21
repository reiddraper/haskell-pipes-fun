{-# LANGUAGE RankNTypes, TypeFamilies #-}
module Main where

import Data.List (sort, foldl')
import Data.Void
import Control.Monad
import qualified Control.Concurrent as Concurrent
import System.IO (Handle)
import Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.Safe as Safe
import qualified Pipes.Safe.Prelude as SafePrelude
import qualified Pipes.Interleave as I
import Test.QuickCheck

-- Pure merge-sort implementation

merge :: (Ord a) => [[a]] -> [a]
merge = foldl' merge' []

merge' :: (Ord a) => [a] -> [a] -> [a]
merge' [] b = b
merge' a [] = a
merge' a@(ahead:atail) b@(bhead:btail) = if ahead < bhead
                                            then ahead:merge' atail b
                                            else bhead:merge' a btail

test :: [[Int]] -> Bool
test xs = merge (map sort xs) == sort (concat xs)

runTests :: IO ()
runTests = quickCheck test

-- Fun pipes

myCat :: (Monad m) => Pipe a a m r
myCat = forever $ do
      x <- await
      yield x

myDelay i = forever $ do
          x <- await
          lift $ Concurrent.threadDelay i
          yield x

-- Pipes merge-sort


mergePipes :: (Monad m, Ord a)
    => (Producer  a m r)
    -> (Producer  a m r)
    -> (Producer  a m r)
mergePipes a b = do
    aEither <- lift $ next a
    case aEither of
        Left _r               -> b
        Right (aHead, aRest) -> do
            bEither <- lift $ next b
            case bEither of
                Left _r               -> a
                Right (bHead, bRest) ->
                    if aHead < bHead
                    then do yield aHead; mergePipes aRest b
                    else do yield bHead; mergePipes a     bRest

pipeTest :: [Int]
pipeTest = P.toList $ each [1..10]

toStdout :: (MonadIO m) => Handle -> Effect m ()
toStdout h = (P.fromHandle h) >-> P.stdoutLn

pathsToProducers :: (Safe.MonadSafe m, Safe.Base m ~ IO)
    => [FilePath]
    -> [Proxy x' x () String m ()]
pathsToProducers paths = map SafePrelude.readFile paths

interleaveFiles :: MonadIO m
    => [Producer String m ()]
    -> Proxy Void () c' c m ()
interleaveFiles sources = (I.interleave compare (sources)) >-> myDelay 250000 >-> P.stdoutLn

main :: IO ()
main = Safe.runSafeT $ runEffect $ interleaveFiles $ pathsToProducers ["/usr/share/dict/web2", "/usr/share/dict/words"]
