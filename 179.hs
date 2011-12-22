module Main where

import qualified Data.List as L

import Debug.Trace

main :: IO ()
main = do
  putStrLn (show $ (last $ take 115000 primes))
  return ()

