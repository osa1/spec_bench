{-# LANGUAGE BangPatterns #-}

module Main where

import Control.DeepSeq
import Criterion
import Criterion.Main

data IntList = ILNil | ILCons {-# UNPACK #-} !Int IntList
  deriving (Show)

instance NFData IntList where
  rnf ILNil        = ()
  rnf (ILCons _ l) = rnf l

main :: IO ()
main = defaultMain
    [ env generateGenericList $ \lst ->
        bgroup "Generic list"
          [ bench "length" (nf length lst)
          , bench "sum" (nf sum lst)
          ]
    , env generateSpecList $ \lst ->
        bgroup "Specialized Int list"
          [ bench "length" (nf specListLength lst)
          , bench "sum" (nf specListSum lst)
          ]
    ]

size :: Int
size = 10 ^ (6 :: Int)

generateGenericList :: IO [Int]
generateGenericList = return [0 .. size]

generateSpecList :: IO IntList
generateSpecList = return $ mkSpecList 0 (size + 1)
  where
    mkSpecList i b
      | i == b    = ILNil
      | otherwise = ILCons i (mkSpecList (i + 1) b)

specListLength :: IntList -> Int
specListLength = specListLengthAcc 0

specListLengthAcc :: Int -> IntList -> Int
specListLengthAcc !acc ILNil = acc
specListLengthAcc !acc (ILCons _ l) = specListLengthAcc (acc + 1) l

specListSum :: IntList -> Int
specListSum lst = specListSumAcc 0 lst

specListSumAcc :: Int -> IntList -> Int
specListSumAcc !acc ILNil = acc
specListSumAcc !acc (ILCons i l) = specListSumAcc (acc + i) l
