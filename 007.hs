
module Main where

import Euler

main :: IO ()
main = do
  putStrLn (show result)
  return ()

result :: Integer
result = primes !! 10000

