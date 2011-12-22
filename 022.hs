import Euler

import Data.Char
import Data.List
import Data.List.Utils

import Debug.Trace

main :: IO ()
main = do
  s <- readFile "names.txt"
  putStrLn (show $ result s)
  return ()

result :: String -> Integer
result s = calcSum $ reverse $ sort $ split "," $ filter (\c -> c /= '"') s

calcSum :: [String] -> Integer
calcSum = snd . (foldr calcScore (1, 0))
  where
    calcScore s (n, r) = (n + 1, r + n * alphaSum s)

alphaSum :: String -> Integer
alphaSum = sum . map (\c -> fromIntegral $ ord c - 64)

