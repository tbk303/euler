import Euler

import Debug.Trace

main :: IO ()
main = do
  r <- return result
  putStrLn (show r)
  return ()

result = head $ filter (\n -> (length $ factors n) > 500) triangles
