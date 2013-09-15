module Main where

import Data.List (sort)
import Pipes
import qualified Pipes.Prelude as P
import Test.QuickCheck

merge :: (Ord a) => [[a]] -> [a]
merge = foldl merge' []

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

main :: IO ()
main = runEffect $ for P.stdinLn (lift . putStrLn)
