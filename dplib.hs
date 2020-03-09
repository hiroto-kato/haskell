{-|
file: DPLib.hs
動的計画法のライブラリ
-}
module DPLib (
  DP
  , dp
  , evalDP, evalDPAll
  ) where

import qualified Data.Map
import State

-- 型定義
type Memo a b = Data.Map.Map a b
type DP a b = a -> State (Memo a b) b

-- |
-- 空メモを返す関数
emptyMemo :: Memo a b
emptyMemo = Data.Map.empty

-- |
-- メモのlookup関数
lookupMemo :: Ord a => a -> Memo a b -> Maybe b
lookupMemo = Data.Map.lookup

-- |
-- メモに追加する関数
insertMemo :: Ord a => a -> b -> Memo a b -> Memo a b
insertMemo = Data.Map.insert

-- |
-- 動的計画法
-- 関数fを受け取り, 引数xをlookupしてメモにあればそれを返し，なければ，計算して結果をinsertしつつ返す
dp :: Ord a => DP a b -> DP a b
dp f x = do
  memo <- gets (lookupMemo x)
  case memo of
    Just y  -> return y
    Nothing -> do
      y <- f x
      modify (insertMemo x y)
      return y

-- |
-- 空のメモから実行して結果を返す関数
evalDP :: DP a b -> a -> b
evalDP f x = evalState (f x) emptyMemo

-- |
-- 複数の引数でメモを使い回して実行する関数
evalDPAll :: DP a b -> [a] -> [b]
evalDPAll f xs = evalState (sequence (map f xs)) emptyMemo

