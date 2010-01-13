
import Euler

main :: IO ()
main = do
  putStrLn (show result)
  return ()

result :: Int
result = result' 0 2 1 1 1901
  where
    result' c _ 1 1 2001 = c
    result' c w d m y = result' c' w' d' m' y'
      where
        (w', d', m', y') = addDay w d m y
        c' = if w == 7 && d == 1 then c + 1 else c

addDay :: Int -> Int -> Int -> Int -> (Int, Int, Int, Int)
addDay w d m y = if monthDays m y == d then (w', 1, m', y') else (w', d + 1, m, y)
  where
    w' = if w == 7 then 1 else w + 1
    (m', y') = if m == 12 then (1, y + 1) else (m + 1, y)

monthDays :: Int -> Int -> Int
monthDays 1 _ = 31
monthDays 2 y = if (y `mod` 4 == 0 && y `mod` 100 /= 0) || (y `mod` 400 == 0) then 29 else 28
monthDays 3 _ = 31
monthDays 4 _ = 30
monthDays 5 _ = 31
monthDays 6 _ = 30
monthDays 7 _ = 31
monthDays 8 _ = 31
monthDays 9 _ = 30
monthDays 10 _ = 31
monthDays 11 _ = 30
monthDays 12 _ = 31

