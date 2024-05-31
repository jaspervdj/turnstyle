#!/usr/bin/env runghc

import Data.List (nub)

gen :: (Enum a, Ord a) => a -> [a] -> Int -> [[a]]
gen _    acc 0 = [acc]
gen zero acc n = do
    x <- nub acc ++ [if null acc then zero else succ (maximum acc)]
    gen zero (acc ++ [x]) (n - 1)

main :: IO ()
main = mapM_ putStrLn $ gen 'A' [] 4
