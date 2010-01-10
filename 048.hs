import Euler

import Data.List
import Data.Function

main :: IO ()
main = do
  putStrLn (show result)
  return ()

result = undigits $ reverse $ take 10 $ reverse $ digits $ foldr (\x r -> r + (x^x)) 0 [1..1000]

