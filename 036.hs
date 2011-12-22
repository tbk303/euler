import Euler

main :: IO ()
main = do
  putStrLn (show result)
  return ()

result :: Integer
result = sum [ x | x <- [1..999999] , (isPalindrome x && isPalindrome (read $ toBinary x))]

