{-# LANGUAGE BangPatterns #-}

module Main where

import Control.DeepSeq

main :: IO ()
main = do
    lst <- force <$> generateSpecList 7
    print (specListLength lst)

data IntList
  = ILNil
  | ILCons {-# UNPACK #-} !Int IntList
  deriving (Show)

instance NFData IntList where
  rnf ILNil        = ()
  rnf (ILCons _ l) = rnf l

generateSpecList :: Int -> IO IntList
generateSpecList size = return $ mkSpecList 0 ((10 ^ size) + 1)
  where
    mkSpecList i b
      | i == b    = ILNil
      | otherwise = ILCons i (mkSpecList (i + 1) b)

specListLength :: IntList -> Int
specListLength = specListLengthAcc 0

specListLengthAcc :: Int -> IntList -> Int
specListLengthAcc !acc ILNil = acc
specListLengthAcc !acc (ILCons _ l) = specListLengthAcc (acc + 1) l
