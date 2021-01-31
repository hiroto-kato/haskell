{- |
Module      : Input
Description :
Copyright   : (c) Hiroto Kato, 2020
License     :
Maintainer  : ff.king.hiroto@gmail.com
Stability   : experimental
Portability : POSIX

ByteStringを使った入力処理用関数
-}

import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS

-- List to Tuple
tuplify2 :: [a] -> (a, a)
tuplify2 (x:y:_) = (x, y)
tuplify2 _ = undefined

--Input functions with ByteString
readInt :: BS.ByteString -> Integer
readInt = fst . fromJust . BS.readInteger
readIntTuple :: BS.ByteString -> (Integer, Integer)
readIntTuple = tuplify2 . map readInt . BS.words
readIntList :: BS.ByteString -> [Integer]
readIntList = map readInt . BS.words

-- Input Integer (cf. >2)
getInt :: IO Integer
getInt = readInt <$> BS.getLine

-- Input Integer List (cf. >2 4)
getIntList :: IO [Integer]
getIntList = readIntList <$> BS.getLine

-- Input Integer NList (cf. >2 1\n2\n)
getIntNList :: Integer -> IO [[Integer]]
getIntNList n = map readIntList <$> replicateM (fromIntegral n) BS.getLine

-- Input Integer Matrix (cf. >2 1\n2\n)
getIntMatrix :: IO [[Integer]]
getIntMatrix = map readIntList . BS.lines <$> BS.getContents

-- Input Integer Tuple (cf. >2 3)
getIntTuple :: IO (Integer, Integer)
getIntTuple = readIntTuple <$> BS.getLine

-- Input Integer NTuple (cf. >2 3\n3 4\n)
getIntNTuples :: Integer -> IO [(Integer, Integer)]
getIntNTuples n = map readIntTuple <$> replicateM (fromIntegral n) BS.getLine

-- Input Integer Tuples (cf. >2 3\n3 4\n)
getIntTuples :: IO [(Integer, Integer)]
getIntTuples = map readIntTuple . BS.lines <$> BS.getContents

main :: IO()
main = do
  --  [n,m] <- getIntList
  n <- getIntTuples
  print n
