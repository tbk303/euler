import Euler

import Debug.Trace

main :: IO ()
main = do
  r <- return result
  putStrLn (show r)
  return ()

result = length $ filter isCircularPrime $ takeWhile ((>) 1000000) primes

isCircularPrime :: Integer -> Bool
isCircularPrime n | n < 10 = True
                  | isCandidate (digits n) = isCircularPrime' $ map undigits $ (rotations $ digits n)
                  | otherwise = False
  where
    isCandidate [] = True
    isCandidate (x:xs) | x `elem` [1,3,7,9] = isCandidate xs
                       | otherwise = False
    isCircularPrime' [] = True
    isCircularPrime' (x:xs) | isPrime x = isCircularPrime' xs
                            | otherwise = False
