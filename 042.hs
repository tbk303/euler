import Euler

import Data.List.Utils

main :: IO ()
main = do
  s <- readFile "words.txt"
  putStrLn (show $ result s)
  return ()

result :: String -> Int
result s = length $ filter isTriangle $ map wordValue $ words
  where
    words = split "," $ filter (\c -> c /= '"') s
