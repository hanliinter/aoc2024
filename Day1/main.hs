module Main where

import Data.Char
import Data.List
import Data.Maybe
import Data.Functor


getNums :: FilePath -> IO [[Int]]
getNums filePath =readFile filePath <&> lines <&> map words <&> map ((map read) :: [String] -> [Int] ) 


solve_part1 :: IO Int
solve_part1= do
  xs <- getNums "input.txt" <&> transpose <&> map sort
  return $ sum $ map abs $ zipWith ( - ) (head xs) (head $ tail xs)

solve_part2 :: IO Int
solve_part2 = do
  xs <- getNums "input.txt" <&> transpose <&> map sort 
  let leftList =  head xs
  let rightList = map (\ys -> (head ys, length ys) ) $ group (head $ tail xs)
  return $ sum $ map (\x -> maybe 0 (x * ) $ lookup x rightList ) leftList


main :: IO ()
main = undefined
