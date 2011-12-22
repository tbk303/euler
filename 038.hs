module Main where

import qualified Data.List as L

import Debug.Trace

main :: IO ()
main = do
  putStrLn (show execute)
  return ()
  
execute :: Integer
execute = maximum $ filter isPandigital $ [ calcConcProd x n | n <- [1..9], x <- filter isDistinct [1..9999] ]

calcConcProd :: Integer -> Integer -> Integer
calcConcProd x n = let r = unDigits 10 $ calcConcProd' n in r
  where
        calcConcProd' 0   = []
        calcConcProd' n' = (calcConcProd' (n' - 1)) ++ (digits 10 (n' * x))

isDistinct :: Integer -> Bool
isDistinct n = let ns = digits 10 n in
  length ns == length (L.nub ns)

isPandigital :: Integer -> Bool
isPandigital n = let ns = digits 10 n in
  [1..(fromIntegral $ length ns)] == (L.sort ns)

-- | Returns the digits of a positive integer as a list, in reverse order.
--   This is slightly more efficient than in forward order.
digitsRev :: Integral n
    => n -- ^ The base to use.
    -> n -- ^ The number to convert to digit form.
    -> [n] -- ^ The digits of the number in list form, in reverse.
digitsRev base i = case i of
        0 -> []
        _ -> lastDigit : digitsRev base rest
    where (rest, lastDigit) = quotRem i base

-- | Returns the digits of a positive integer as a list.
digits :: Integral n
    => n -- ^ The base to use (typically 10).
    -> n -- ^ The number to convert to digit form.
    -> [n] -- ^ The digits of the number in list form.
digits base = reverse . digitsRev base

-- | Takes a list of digits, and converts them back into a positive integer.
unDigits :: Integral n
    => n -- ^ The base to use.
    -> [n] -- ^ The digits of the number in list form.
    -> n -- ^ The original number.
unDigits base = foldl (\ a b -> a * base + b) 0

