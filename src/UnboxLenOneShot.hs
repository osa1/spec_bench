module Main where

import Control.DeepSeq

import Types

main :: IO ()
main = do
    lst <- force <$> generateSpecList 7
    print (specListLength lst)
