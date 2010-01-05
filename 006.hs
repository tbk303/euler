
module Main where

import Euler

main :: IO ()
main = do
  putStrLn (show result)
  return ()

result :: Integer
result = squareSum - sumSquare
  where
    squareSum = (sum [1..100]) ^ 2
    sumSquare = sum $ map (flip (^) 2) [1..100]

