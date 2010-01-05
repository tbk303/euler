
module Euler 
  ( primes
  , factor
  , isPalindrome
  ) where

isPalindrome :: Integer -> Bool
isPalindrome n = let s = show n in s == (reverse s)

factor :: Integer -> Integer -> Integer
factor a b = if b == 0 then a else factor b (a `mod` b)

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
