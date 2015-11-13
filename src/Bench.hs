module Main where

import Criterion
import Criterion.Main

import Types

main :: IO ()
main = defaultMain $ concatMap mkbench [1..7 :: Int]
-- main = defaultMain $ concatMap mkbench [7]

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
