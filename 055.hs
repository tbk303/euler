module Main where

import Euler

main :: IO ()
main = do
  putStrLn (show execute)
  return ()

execute :: Int
execute = length [ x | x <- [0..10000], isLychrel x ]

isLychrel :: Integer -> Bool
isLychrel = isLychrel' 1 . reverseAdd
  where
        isLychrel' 50 _ = True
        isLychrel' c n  = isLychrel' (c + 1) (reverseAdd n) && (not $ isPali (digits n))

reverseAdd :: Integer -> Integer
reverseAdd n = n + (undigits . reverse . digits $ n)

isPali :: Eq a => [a] -> Bool
isPali xs = xs == reverse xs
