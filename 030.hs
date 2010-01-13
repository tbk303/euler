import Euler

main :: IO ()
main = do
  putStrLn (show result)
  return ()

result :: Integer
result = sum $ filter isPowerSum [10..999999]

isPowerSum :: Integer -> Bool
isPowerSum n = n == (sum $ map (^5) $ digits n)

