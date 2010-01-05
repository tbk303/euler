import Euler

main :: IO ()
main = do
  putStrLn (show result)
  return ()

result :: Integer
result = sum $ digits (2^1000)

