{- |
Module      : fib
Description :
Copyright   : (c) Hiroto Kato, 2020
License     :
Maintainer  : ff.king.hiroto@gmail.com
Stability   : experimental
Portability : POSIX

I make a Fibonacci sequence using dynamic programming.
-}

import DPLib

main :: IO()
main = do
  
fib :: Int -> Integer
fib 0 = DPLib.evalDP fibSub 

fibSub :: DP Int Integer
fibSub = dp $ \n ->
  if n <= 1
  then return (toInteger n)
  else do
    a <- fibSub (n - 2)
    b <- fibSub (n - 1)
    return (a + b)
