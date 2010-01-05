
module Main where

import Euler

main :: IO ()
main = do
  putStrLn (show result)
  return ()

result :: Integer
result = head $ dropWhile (not . isDivisible) [1..]

isDivisible :: Integer -> Bool
isDivisible n = isDivisible' [2..20]
  where
    isDivisible' [] = True
    isDivisible' (c:cs) = if n `mod` c /= 0 then False else isDivisible' cs
