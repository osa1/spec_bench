{-# LANGUAGE BangPatterns #-}

module Main where

import Control.DeepSeq
import Criterion
import Criterion.Main

main :: IO ()
main = defaultMain $ concatMap mkbench [1..7 :: Int]
-- main = defaultMain $ concatMap mkbench [6]

mkbench :: Int -> [Benchmark]
mkbench size =
    [ env (generateBaseList size) $ \lst ->
        bgroup ("Haskell list (size: 10^" ++ show size ++ ")")
          [ bench "length" (nf length lst)
          , bench "sum" (nf sum lst)
          ]

    , env (generateGL size) $ \lst ->
        bgroup ("Generic list (size: 10^" ++ show size ++ ")")
          [ bench "length" (nf glLength lst)
          , bench "sum" (nf glSum lst)
          ]

    , env (generateSGL size) $ \lst ->
        bgroup ("Generic strict list (size: 10^" ++ show size ++ ")")
          [ bench "length" (nf sglLength lst)
          , bench "sum" (nf sglSum lst)
          ]

    , env (generateSpecList size) $ \lst ->
        bgroup ("Unboxed Int list (size: 10^" ++ show size ++ ")")
          [ bench "length" (nf specListLength lst)
          , bench "sum" (nf specListSum lst)
          ]
    ]

--------------------------------------------------------------------------------
-- * Haskell's list

generateBaseList :: Int -> IO [Int]
generateBaseList size = return [0 .. (10 ^ size)]

--------------------------------------------------------------------------------
-- * Operations on lazy generic lists

data GL a = GLNil | GLCons a (GL a)
  deriving (Show)

instance NFData a => NFData (GL a) where
  rnf GLNil        = ()
  rnf (GLCons h t) = rnf h `seq` rnf t

generateGL :: Int -> IO (GL Int)
generateGL size = return $ foldr GLCons GLNil [0 .. (10 ^ size)]

glLength :: GL a -> Int
glLength = glLengthAcc 0

glLengthAcc :: Int -> GL a -> Int
glLengthAcc !acc GLNil        = acc
glLengthAcc !acc (GLCons _ l) = glLengthAcc (acc + 1) l

glSum :: GL Int -> Int
glSum = glSumAcc 0

glSumAcc :: Int -> GL Int -> Int
glSumAcc !acc GLNil        = acc
glSumAcc !acc (GLCons h t) = glSumAcc (acc + h) t

--------------------------------------------------------------------------------
-- * Strict generic lists

data SGL a = SGLNil | SGLCons !a !(SGL a)
  deriving (Show)

instance NFData a => NFData (SGL a) where
  rnf SGLNil        = ()
  rnf (SGLCons h t) = rnf h `seq` rnf t

generateSGL :: Int -> IO (SGL Int)
generateSGL size = return $ foldr SGLCons SGLNil [0 .. (10 ^ size)]

sglLength :: SGL a -> Int
sglLength = sglLengthAcc 0

sglLengthAcc :: Int -> SGL a -> Int
sglLengthAcc !acc SGLNil        = acc
sglLengthAcc !acc (SGLCons _ l) = sglLengthAcc (acc + 1) l

sglSum :: SGL Int -> Int
sglSum = sglSumAcc 0

sglSumAcc :: Int -> SGL Int -> Int
sglSumAcc !acc SGLNil        = acc
sglSumAcc !acc (SGLCons h t) = sglSumAcc (acc + h) t

--------------------------------------------------------------------------------
-- * Specialized strict lists

data IntList
  = ILNil
  | ILCons {-# UNPACK #-} !Int {-# UNPACK #-} !IntList
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

specListSum :: IntList -> Int
specListSum = specListSumAcc 0

specListSumAcc :: Int -> IntList -> Int
specListSumAcc !acc ILNil = acc
specListSumAcc !acc (ILCons i l) = specListSumAcc (acc + i) l
