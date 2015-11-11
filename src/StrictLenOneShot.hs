{-# LANGUAGE BangPatterns #-}

module Main where

import Control.DeepSeq

main :: IO ()
main = do
    lst <- force <$> generateSGL 7
    print (sglLength lst)

data SGL a = SGLNil | SGLCons !a (SGL a)
  deriving (Show)

instance NFData a => NFData (SGL a) where
  rnf SGLNil        = ()
  rnf (SGLCons h t) = rnf h `seq` rnf t

generateSGL :: Int -> IO (SGL Int)
generateSGL size = return $ foldr SGLCons SGLNil [0 .. (10 ^ size) - 1]

sglLength :: SGL a -> Int
sglLength = sglLengthAcc 0

sglLengthAcc :: Int -> SGL a -> Int
sglLengthAcc !acc SGLNil        = acc
sglLengthAcc !acc (SGLCons _ l) = sglLengthAcc (acc + 1) l
