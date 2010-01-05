
module Main where

import Euler

main :: IO ()
main = do
  putStrLn (show result)
  return ()

n = 600851475143

result :: Integer
result = result' 0 (takeWhile (\x -> x < (floor $ sqrt $ fromIntegral n)) primes)
  where
    result' x [] = x
    result' x (c:cs) = if n `div` c == 0 then result' c cs else result' x cs

