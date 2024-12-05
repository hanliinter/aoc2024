module Main where

import Data.Functor
import Data.List

rotate1 :: [a] -> [a]
rotate1 = drop 1 <> take 1

deleteAt n xs = let (left,right) = splitAt n xs
                  in
                  case (left,right) of
                    ([],right) -> right
                    (left,[])  -> if n == length xs then reverse (tail (reverse left)) else left
                    (left,right) -> reverse (tail (reverse left)) ++ right

getData :: String -> IO [[Int]]
getData fp = readFile fp <&> lines <&> map ((map read) .words)


solve_part1 :: [[Int]] -> Int
solve_part1 xss = length $ filter (==True) $  map solveRow xss

solve_part2 :: [[Int]] -> Int
solve_part2 xss = length $ filter (==True) $  map solveRowWithDampener xss


solveRow :: [Int] -> Bool
solveRow xs = let target = zipWith(-) xs (tail xs)
                       in
                        (all (>0) target || all (<0) target) && (all (<=3) $ (map abs target))

solveRowWithDampener :: [Int] -> Bool
solveRowWithDampener xs = let xss = [deleteAt n xs | n <- [1..(length xs)]]
                          in
                            solveRow xs || any (solveRow) xss

                        

main = getData "input.txt" <&> solve_part2 >>= print
