import Euler

import Data.List
import Data.Function

main :: IO ()
main = do
  putStrLn (show result)
  return ()

result = getPos 0 fibs
  where
    getPos c (n:ns) | (length $ digits n) >= 1000 = c
                    | otherwise                 = getPos (c + 1) ns
