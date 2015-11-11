{-# LANGUAGE BangPatterns #-}

module Main where

import Control.DeepSeq

main :: IO ()
main = do
    lst <- force <$> generateGL 7
    print (glLength lst)

data GL a = GLNil | GLCons a (GL a)
  deriving (Show)

instance NFData a => NFData (GL a) where
  rnf GLNil        = ()
  rnf (GLCons h t) = rnf h `seq` rnf t

generateGL :: Int -> IO (GL Int)
generateGL size = return $ foldr GLCons GLNil [0 .. (10 ^ size) - 1]

glLength :: GL a -> Int
glLength = glLengthAcc 0

glLengthAcc :: Int -> GL a -> Int
glLengthAcc !acc GLNil        = acc
glLengthAcc !acc (GLCons _ l) = glLengthAcc (acc + 1) l
