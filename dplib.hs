 -- file: dplib.hs

module dplib where

changeMin :: a -> a -> a
changeMin x y = if x > y then y
                else x

changeMax :: a -> a -> a
changeMax x y = if x > y then x
                else y
