import Data.Array

fibonacci :: Integer -> Integer
fibonacci n = memo!n
where
  memo = array (0, n) [ (i, fib i) | i <- [0..n] ]
  fib 0 = 0
  fib 1 = 1
  fib i = memo!(i-1) + memo!(i-2)
