
module Main where

import Euler

main :: IO ()
main = do
  putStrLn (show result)
  return ()

result :: Integer
result = maximum $ filter isPalindrome [ x * y | x <- [100..999], y <- [100..999] ]

