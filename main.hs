module Main where

import Data.List (sort)
import Test.QuickCheck

mergeSort :: (Ord a) => [a] -> [a] -> [a]
mergeSort [] b = b
mergeSort a [] = a
mergeSort a@(ahead:atail) b@(bhead:btail) = if ahead < bhead
                                            then ahead:mergeSort atail b
                                            else bhead:mergeSort a btail

test :: [Int] -> [Int] -> Bool
test a b = mergeSort x y == sort (x ++ y)
        where x = sort a
              y = sort b

main :: IO ()
main = quickCheck test
