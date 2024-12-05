module Main where

import Data.Char
import Data.Functor
import Data.List
import Data.Maybe

countOccurLine :: String -> String -> Int
countOccurLine needle haystack  = go needle haystack 0
  where go :: String -> String -> Int -> Int
        go needle [] acc = acc
        go needle haystack@(h:rest) acc = if isPrefixOf needle haystack then go needle rest (acc+1) else go needle rest acc

countOccur :: String -> [String] -> Int
countOccur needle haystacks = sum $ map (countOccurLine needle ) haystacks

type Pos = (Int,Int)
type Dict = [(Pos, Char)]

markPos :: [String] -> Dict
markPos xss = concatMap (\(i,cs) -> map (\(j,c) -> ((i,j),c)) $ zip [1..] cs) $ zip [1..] xss  


diagonal :: Int -> [[Pos]]
diagonal n = map (\x -> diagonalLine n x) [2..n+n]
  where diagonalLine n s = [(j,i)| i <- [1..n] , j <-[1..n], i+j == s ]

getDiagonalStrings :: Dict -> Int -> [String]
getDiagonalStrings dict n = let ns = diagonal n
                             in
                              map (\ps -> map (\p -> case lookup p dict of
                                            Nothing -> error "should never happen"
                                            Just c -> c) ps ) ns


solve_part1 :: [String] -> Int
solve_part1 xss = let revXss = map reverse xss  -- horizontally backwards
                      n = length xss
                      transXss = transpose xss   -- vertically forwards 
                      primaryDiagonalOrig = getDiagonalStrings (markPos xss) n
                      antiDiagonalOrig = getDiagonalStrings (markPos revXss) n
                      t = [xss, transXss,primaryDiagonalOrig,antiDiagonalOrig]
                      n1 =  sum $ map (countOccur "SAMX") t
                      n2 =  sum $ map (countOccur "XMAS") t
                    in
                     n1 + n2
getData :: String -> IO [String]
getData fp = readFile fp <&> lines


verifyX :: Dict -> Pos -> Int -> Bool
verifyX dict (i,j) n = if
                         i == 1 || i == n
                         || j == 1 || j == n
                       then False
                       else -- ugly, should rewrite
                         let lu = maybe (error "Should never happen") id $ lookup (i-1,j-1) dict
                             ru = maybe (error "Should never happen") id $ lookup (i-1,j+1) dict
                             lb = maybe (error "Should never happen") id $ lookup (i+1,j-1) dict
                             rb = maybe (error "Should never happen") id $ lookup (i+1,j+1) dict
                         in
                           case (lu,ru,lb,rb) of
                             ('M','M','S','S') -> True
                             ('S','S','M','M') -> True
                             ('M','S','M','S') -> True
                             ('S','M','S','M') -> True
                             _  -> False       


solve_part2 xss = let dict = markPos xss
                      n = length xss
                   in length $ filter (\(p,c) -> c == 'A' && verifyX dict p n) dict
                      



main = getData "input.txt" <&> solve_part2 >>= print
