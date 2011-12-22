module Main where

import Debug.Trace

import Control.Parallel

import Data.List

import Euler

main :: IO ()
main = do
  putStrLn (show execute)
  return ()

nMax = 10000

execute :: Int
execute =  odd `par` even `par` odd + even
  where
        even = length [ x | x <- [2,6..nMax], hasOnlyOneTriangle x ]
        odd = length [ x | x <- [4,8..nMax], hasOnlyOneTriangle x ]

hasOnlyOneTriangle :: Int -> Bool
hasOnlyOneTriangle n = length (trace (show n ++ ": " ++ show triangles) triangles) == 1
  where
        triangles = let m = ceiling (fromIntegral n / 2) in
          nub $ map sort [ [a, b, c] | a <- [1..m], b <- [1..m], c <- [1..m], a+b+c == n && a ^ 2 + b ^ 2 == c ^ 2 ]
