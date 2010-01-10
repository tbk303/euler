import Euler

import Data.List
import Data.Function

main :: IO ()
main = do
  putStrLn (show result)
  return ()

result = fst $ head $ reverse $ sortBy (compare `on` snd) $ counts
  where
    counts = [ (x, collatzCount x) | x <- [1..1000000] ]

-- Count the steps until 1 is reached
collatzCount :: Integer -> Integer
collatzCount = collatzCount' 0
  where
    collatzCount' c 1 = c
    collatzCount' c n = collatzCount' (c + 1) (collatz n)

-- Calculate one step in a Collatz chain
collatz :: Integer -> Integer
collatz n | even n = n `div` 2
          | odd n  = 3 * n + 1

