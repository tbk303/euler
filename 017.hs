import Euler

import Data.Maybe

main :: IO ()
main = do
  putStrLn (show result)
  return ()

result :: Int
result = length $ concat $ map spelling [1..1000]

spelling :: Int -> String
spelling n = aboveHundred ++ and ++ belowHundred
  where
    and = if aboveHundred /= "" && belowHundred /= "" then "and" else ""
    aboveHundred = (format nth "thousand") ++ (format nhu "hundred")
    belowHundred = fromMaybe ((tens nte) ++ (first nsi)) (special $ n `mod` 100)
    nsi = n `mod` 10
    nte = (n `mod` 100) `div` 10
    nhu = (n `mod` 1000) `div` 100
    nth = (n `mod` 10000) `div` 1000

format :: Int -> String -> String
format 0 _ = ""
format n s = first n ++ s

first :: Int -> String
first 1 = "one"
first 2 = "two"
first 3 = "three"
first 4 = "four"
first 5 = "five"
first 6 = "six"
first 7 = "seven"
first 8 = "eight"
first 9 = "nine"
first _ = ""

special :: Int -> Maybe String
special 11 = Just "eleven"
special 12 = Just "twelve"
special 13 = Just "thirteen"
special 14 = Just "fourteen"
special 15 = Just "fifteen"
special 16 = Just "sixteen"
special 17 = Just "seventeen"
special 18 = Just "eighteen"
special 19 = Just "nineteen"
special _ = Nothing

tens :: Int -> String
tens 1 = "ten"
tens 2 = "twenty"
tens 3 = "thirty"
tens 4 = "forty"
tens 5 = "fifty"
tens 6 = "sixty"
tens 7 = "seventy"
tens 8 = "eighty"
tens 9 = "ninety"
tens _ = ""

