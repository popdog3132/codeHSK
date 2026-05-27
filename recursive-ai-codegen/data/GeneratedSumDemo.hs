module Main where

generatedSum :: [Int] -> Int
generatedSum nums = sum nums

main :: IO ()
main =
  if and
       [ generatedSum [] == 0
       , generatedSum [1,2,3] == 6
       , generatedSum [10,-2,5] == 13
       ]
    then putStrLn "PASS"
    else putStrLn "FAIL"
