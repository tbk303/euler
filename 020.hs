import Euler

main :: IO ()
main = do
  putStrLn (show result)
  return ()

result :: Integer
result = sum $ digits $ factorial 100
