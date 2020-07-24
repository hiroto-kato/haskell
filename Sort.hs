{- |
Module      : Sort
Description :
Copyright   : (c) Hiroto Kato, 2020
License     :
Maintainer  : ff.king.hiroto@gmail.com
Stability   : experimental
Portability : POSIX

Sort Algorithm
-}
module Sort where

-- | Quick Sort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let
    -- xより小さい集合
    smallerSorted = quicksort [a | a <- xs, a <= x]
    -- xより大きい集合
    biggerSorted = quicksort [a | a <- xs, a > x]
  in
    smallerSorted ++ [x] ++ biggerSorted

-- | Bubble Sort
bubblesort :: (Ord a) => [a] -> [a]
bubblesort [] = []
bubblesort xs =
  let
    -- 並びかえる
    sorted = subBubble xs
    -- 最後の要素を取り出す
    last_value = last sorted
  in
    -- 最後の要素は並び替えが完了している
    bubblesort (init sorted) ++ [last_value]

-- | subBubble
-- 1ステップのバブルソートを行う
subBubble :: (Ord a) => [a] -> [a]
subBubble [] = []
subBubble [y1] = [y1]
subBubble (y1:y2:ys) = if y1 > y2 then y2:subBubble (y1:ys)
                       else y1:subBubble (y2:ys)

-- | Insertion Sort
insertionSort :: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insertioSort (insert xs x)

-- | insert
insert :: (Ord a) => [a] -> a -> [a]
insert [] z = []
insert [x] z = if x < z then x:[z] else [z]
insert (x:y:xs) z = if x <= z && z < y
                    then x:z:y:xs
                    else x:insert (y:xs) z

