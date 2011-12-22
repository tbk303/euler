import Euler

import Debug.Trace

main :: IO ()
main = do
  r <- return result
  putStrLn (show r)
  putStrLn (show $ sum r)
  return ()

result = checkTruncatable [] (drop 4 primes)

checkTruncatable :: [Integer] -> [Integer] -> [Integer]
checkTruncatable rs (p:ps) | length rs == 11 = rs
                           | otherwise       = if isTruncatable p then checkTruncatable (p:rs) ps 
                                               else checkTruncatable rs ps

isTruncatable :: Integer -> Bool
isTruncatable n = let ns = digits n in isTruncFwd ns && isTruncRev ns
  where
    isTruncFwd (x:[]) = isPrime x
    isTruncFwd (_:xs) = isPrime (undigits xs) && isTruncFwd xs
    isTruncRev (x:[]) = isPrime x
    isTruncRev xs = let xs' = reverse $ drop 1 $ reverse xs in 
                    isPrime (undigits xs') && isTruncRev xs'
  


