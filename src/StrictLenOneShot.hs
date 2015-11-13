module Main where

import Control.DeepSeq

import Types

main :: IO ()
main = do
    lst <- force <$> generateSGL 7
    print (sglLength lst)
