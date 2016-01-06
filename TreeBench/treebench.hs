-- |

module Main where

import Control.Exception
import Control.Monad
import Data.Time.Clock
import System.Environment

-- Strict vversion
--------------------------------------------------------------------------------

data Tree = Leaf {-# UNPACK #-} !Int
          | Node !Tree !Tree

-- | Build a fully-evaluated tree
buildTree :: Int -> IO Tree
buildTree n = evaluate $ go 1 n
  where
  go root 0 = Leaf root
  go root n = Node (go root (n-1))
                   (go (root + 2^(n-1)) (n-1))

add1Tree :: Tree -> Tree
add1Tree (Leaf n)   = Leaf (n+1)
add1Tree (Node x y) = Node (add1Tree x) (add1Tree y)

leftmost (Leaf n) = n
leftmost (Node x _) = leftmost x

--------------------------------------------------------------------------------

bench :: Tree -> IO Tree
bench tr = evaluate (add1Tree tr)

main =
 do args <- getArgs
    let power = case args of
                  [p] -> read p
                  _   -> error $ "Bad command line args.  Expected one number (exponent): " ++show args
    forM_ [1..10] $ \_ -> do
      tr  <- buildTree power
      t1  <- getCurrentTime
      tr2 <- bench tr
      t2  <- getCurrentTime
      putStrLn $ "Test, leftmost leaf in output: " ++ show (leftmost tr2)
      putStrLn $ "Took "++ show (diffUTCTime t2 t1)
