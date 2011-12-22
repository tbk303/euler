
module Euler 
  ( primes
  , fibs
  , triangles
  , pentagonals
  , factor
  , factorial
  , factors
  , digits
  , undigits
  , rotations
  , wordValue
  , toBinary
  , isTriangle
  , isPrime
  , isPalindrome
  , isPythagoreanTriplet
  ) where

import Data.List
import Data.Char

wordValue :: String -> Integer
wordValue s = sum $ map (\c -> fromIntegral $ (ord c) - 64) s

toBinary :: Integer -> String
toBinary n
        | n < 2 = show n
        | otherwise = toBinary (n `div` 2) ++ (show (n `mod` 2))

rotations :: [a] -> [[a]]
rotations [] = []
rotations n = rotations' [n] n
  where
    rotations' r (n:[]) = r
    rotations' (r:rs) (n:ns) = rotations' (((tail r) ++ [n]):r:rs) ns

fibs :: [Integer]
fibs = 0 : 1 : (zipWith (+) fibs (tail fibs))

triangles :: [Integer]
triangles = 1 : 3 : (zipWith (+) (tail triangles) [3..])

pentagonals :: [Integer]
pentagonals = 1 : 5 : (zipWith (\p n -> p + 3 * n - 2) (tail pentagonals) [3..])

isPythagoreanTriplet :: Integer -> Integer -> Integer -> Bool
isPythagoreanTriplet a b c = (a < b) && (b < c) && (a^2 + b^2 == c^2)

isPalindrome :: Integer -> Bool
isPalindrome n = let s = show n in s == (reverse s)

factor :: Integer -> Integer -> Integer
factor a b = if b == 0 then a else factor b (a `mod` b)

factorial :: Integer -> Integer
factorial 0             = 1
factorial 1             = 1
factorial 2             = 2
factorial 3             = 6
factorial 4             = 24
factorial 5             = 120
factorial 6             = 720
factorial 7             = 5040
factorial 8             = 40320
factorial 9             = 362880
factorial n | n > 1     = n * factorial (n - 1)
            | otherwise = error $ "Factorial only defined for positive numbers: " ++ (show n)

factors :: Integral a => a -> [a]
factors i = factors' i 1 []
  where
    factors' n c cs = if c >= n then cs else res
      where
        res = let (d, r) = divMod i c in
              if r == 0 then factors' d (c+1) (if c == d then c:cs else c:d:cs) else factors' n (c+1) cs

isPrime :: Integer -> Bool
isPrime 2 = True
isPrime n = odd n && n `elemSorted` primes

isTriangle :: Integer -> Bool
isTriangle n = n `elemSorted` triangles

elemSorted :: (Eq a, Ord a) => a -> [a] -> Bool
elemSorted x (y:ys) | x < y = False
                    | x == y = True
                    | x > y = elemSorted x ys

primes :: [Integer]
primes = 2:3:5:7:primes' 
 where
  primes' = [11,13] ++ drop 2 (rollFrom 11) `minus` comps
  mults   = map (\p-> fromList $ map (p*) $ rollFrom p) $ primes'
  comps   = fst $ tfold mergeSP (pairwise mergeSP mults)
  fromList (x:xs) = ([x],xs)
  rollFrom n      = let x = (n-11) `mod` 210
                        (y,_) = span (< x) wheelNums
                    in roll n $ drop (length y) wheel

wheelNums = roll 0 wheel
roll      = scanl (+)
wheel     = 2:4:2:4:6:2:6:4:2:4:6:6:2:6:4:2:6:4:6:8:4:2:4:2:
            4:8:6:4:6:2:4:6:2:6:6:4:2:4:6:2:6:4:2:4:2:10:2:10:wheel

pairwise f (x:y:ys)  = f x y : pairwise f ys

tfold f (a: ~(b: ~(c:xs)))
                     = (a `f` (b `f` c)) `f` tfold f (pairwise f xs)

mergeSP (a,b) ~(c,d) = let (bc,b') = spMerge b c
                       in (a ++ bc, merge b' d)
 where 
  spMerge :: (Ord a) => [a] -> [a] -> ([a],[a]) 
  spMerge a@(x:xs) b@(y:ys) = case compare x y of
          LT ->  (x:c,d)  where (c,d) = spMerge xs b
          EQ ->  (x:c,d)  where (c,d) = spMerge xs ys
          GT ->  (y:c,d)  where (c,d) = spMerge a  ys
  spMerge a [] = ([] ,a)
  spMerge [] b = ([] ,b)

minus :: (Ord a) => [a] -> [a] -> [a]
minus a@(x:xs) b@(y:ys) = case compare x y of 
                            LT -> x: xs `minus` b
                            EQ ->    xs `minus` ys
                            GT ->    a  `minus` ys
minus a        b        = a

merge a@(x:xs) b@(y:ys) = case compare x y of 
                            LT -> x: merge xs b 
                            EQ -> x: merge xs ys
                            GT -> y: merge a  ys
merge a        b        = if null a then b else a

digitsRev :: Integral n => n -> n -> [n]
digitsRev base i = case i of
        0 -> []
        _ -> lastDigit : digitsRev base rest
    where (rest, lastDigit) = quotRem i base

digits :: Integral n => n -> [n]
digits = reverse . digitsRev 10

undigits :: Integral n => [n] -> n
undigits = foldl (\ a b -> a * 10 + b) 0


