import Euler

main :: IO ()
main = do
  putStrLn (show result)
  return ()

result :: Integer
result = let (a,b,c) = head $ filter isValid [ (a,b,c) | a <- [1..1000], b <- [1..1000], c <- [1..1000] ] in a * b * c
  where
    isValid (a,b,c) = isPythagoreanTriplet a b c && (a + b + c == 1000)

