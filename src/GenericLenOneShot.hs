module Main where

import Control.DeepSeq

import Types

main :: IO ()
main = do
    lst <- force <$> generateGL 7
    print (glLength lst)
