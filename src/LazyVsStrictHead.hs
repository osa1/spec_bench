{-# LANGUAGE BangPatterns #-}

module Main where

import System.Clock
import System.Mem (performMajorGC)

import Types

mAX_FIB :: Int
mAX_FIB = 8

lIST_LEN :: Int
lIST_LEN = 10 ^ 7

inefficientFib :: Int -> Int
inefficientFib i
  | i == 0    = 1
  | i == 1    = 1
  | otherwise = inefficientFib (i - 1) + inefficientFib (i - 2)

generateFibGL :: Int -> GL Int
generateFibGL len
  | len == 0  = GLNil
  | otherwise =
    -- We use mod here to avoid floating out the fib call
    -- (which would do memoization)
    inefficientFib (len `mod` mAX_FIB) `GLCons` generateFibGL (len - 1)

generateFibSGL :: Int -> SGL Int
generateFibSGL len
  | len == 0  = SGLNil
  | otherwise =
    inefficientFib (len `mod` mAX_FIB) `SGLCons` generateFibSGL (len - 1)

gc :: IO ()
gc = do
  putStrLn "Performing major GC"
  performMajorGC

main :: IO ()
main = do
  gc
  putStrLn "Generating generic list"
  !t1 <- getTime Monotonic
  let lst  = generateFibGL lIST_LEN
      !len = glLength lst
  !t2 <- getTime Monotonic

  putStrLn $ "Took " ++ showTimeSpec (t2 `diffTimeSpec` t1)

  gc
  putStrLn "Summing generic list"
  !t3 <- getTime Monotonic
  let !sum = glSum lst
  !t4 <- getTime Monotonic

  putStrLn $ "Took " ++ showTimeSpec (t4 `diffTimeSpec` t3)


  gc
  putStrLn "Generating strict list"
  !t5 <- getTime Monotonic
  let slst  = generateFibSGL lIST_LEN
      !slen = sglLength slst
  !t6 <- getTime Monotonic

  putStrLn $ "Took " ++ showTimeSpec (t6 `diffTimeSpec` t5)

  gc
  putStrLn "Summing strict list"
  !t7 <- getTime Monotonic
  let !ssum = sglSum slst
  !t8 <- getTime Monotonic

  putStrLn $ "Took " ++ showTimeSpec (t8 `diffTimeSpec` t7)

showTimeSpec :: TimeSpec -> String
showTimeSpec (TimeSpec s ns) = show s ++ "." ++ zeroes 9 (show ns) ++ " seconds."

zeroes :: Int -> String -> String
zeroes n str = replicate (n - length str) '0' ++ str
