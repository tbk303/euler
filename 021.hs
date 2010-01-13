import Euler

import Data.List

import qualified Data.IntSet as IS

main :: IO ()
main = do
  putStrLn (show result)
  return ()

result :: Int
result = sum $ generateAmicables 9999

generateAmicables :: Int -> [Int]
generateAmicables max = IS.toList $ foldr checkAmicable IS.empty [1..max]
  where
    checkAmicable a rs = let b = d a in if IS.member a rs then rs else
                                 if (d b == a && a /= b) then IS.insert a (IS.insert b rs) else rs

d :: Int -> Int
d n = sum $ delete n $ factors (fromIntegral n)
