module Main where

import Data.Char
import Data.List
import Data.Functor
import Debug.Trace

getData :: String -> IO String
getData fp = readFile fp 

-- I should not use 'clever' trick but instead create a very simple parser
-- mul instruction contains 'mul', '(', 1-3 digits, ',' 1-3 digits ')'
-- mul ( 2 , 4 ) do nothing
-- maybe I should not do a parser

data Mul = Mul Int Int
         | DO
         | DONT deriving (Show, Eq)

-- chop :: String -> String
-- chop xs = go [] [] xs
--   where go result _ [] = result
        
isValidDigits :: String -> Bool
isValidDigits xs = all (isDigit) xs && length xs <= 3

isValidOperant :: String -> Bool
isValidOperant xs = let (right,left) = break (==',') xs
                     in
                      case (right,left) of
                        (_,[]) -> False
                        ([],_) -> False
                        (right,(x:left)) | x == ','  -> isValidDigits right && isValidDigits left
                                         |otherwise -> False
parse :: String -> [Mul]
parse xs = go [] [] xs
  where go result _ [] = result -- even if there is still something in stack we should ignore it
        go result stack (x:xs)  | isDigit x = let leftPart = takeWhile (isDigit) stack in
                                                if isValidDigits (x:leftPart) then go result (x:stack) xs else go result [] xs

                                | x == 'm' = go result [x] xs
                                | x == 'u' = if isPrefixOf "m" stack then go result (x:stack) xs else go result [] xs
                                | x == 'l' = if isPrefixOf "um" stack then go result (x:stack) xs else go result [] xs
                                | x == 'd' = go result [x] xs
                                | x == 'o' = if isPrefixOf "d" stack then go result (x:stack) xs else go result [] xs                                
                                | x == 'n' = if isPrefixOf "od" stack then go result (x:stack) xs else go result [] xs
                                | x == '\'' = if isPrefixOf "nod" stack then go result (x:stack) xs else go result [] xs
                                | x == 't' = if isPrefixOf "'nod" stack then go result (x:stack) xs else go result [] xs
                                | x == '(' = if (isPrefixOf "lum" stack || isPrefixOf "od" stack || isPrefixOf "t'nod" stack) then
                                               go result (x:stack) xs
                                             else
                                               go result [] xs
                                | x == ',' = let leftPart = takeWhile (isDigit) stack
                                               in
                                               if isValidDigits leftPart then go result (x:stack) xs else go result [] xs
                                | x == ')' = let (leftPart, rightPart) = span (/='(') stack
                                             in
                                               case rightPart of
                                                 ('(':'l':'u':'m':_) -> if isValidOperant leftPart then
                                                                          let (left,_:right) = break (==',') $ reverse leftPart
                                                                              item = Mul (read left) (read right)
                                                                          in
                                                                            go  (item:result) [] xs
                                                                        else
                                                                          go result [] xs
                                                 ('(':'o':'d':_) -> go (DO:result) [] xs
                                                 ('(':'t':'\'':'n':'o':'d':_) -> go (DONT:result) [] xs
                                                 _ -> go result [] xs
                                | otherwise = go result [] xs
                                          

solve_part1 :: String -> Int
solve_part1 xs = let muls = parse xs
                  in
                  sum $ map (\(Mul l r) -> l * r) muls

solve_part2 :: String -> Int
solve_part2 xs = go True 0 $ reverse $ parse xs
  where go :: Bool -> Int -> [Mul] -> Int
        go _ acc [] = acc
        go _ acc (DO:rest) = go True acc rest
        go _ acc (DONT:rest) = go False acc rest
        go True acc ((Mul l r):rest) = go True (acc + (l * r)) rest
        go False acc ((Mul l r):rest) = go False acc rest


main :: IO ()
main = getData "input.txt"  <&> solve_part2  >>= print

debug = getData "sample.txt"  <&> reverse. parse  
