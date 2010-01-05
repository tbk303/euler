import Euler

main :: IO ()
main = do
  putStrLn (show result)
  return ()

result :: Integer
result = sum $ takeWhile (\n -> n < 2000000) primes 

