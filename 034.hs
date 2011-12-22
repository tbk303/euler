import Euler

main :: IO ()
main = do
  putStrLn (show result)
  return ()

-- Stupid here, as we couldn't come up with a definition of the upper bound
result = sum $ filter isFactorialSumEqual [10..999999]

isFactorialSumEqual :: Integer -> Bool
isFactorialSumEqual n = n == (sum $ map factorial $ digits n)
