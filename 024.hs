import Euler

import Data.List

main :: IO ()
main = do
  putStrLn (show result)
  return ()

result :: Integer
result = read $ (sort $ permutations "0123456789") !! 999999

