module Main where

import Data.List
import Data.Functor 
import qualified Data.Map as M


wordsWhile :: (a->Bool) -> [a] -> [[a]]
wordsWhile p xs = case dropWhile p xs of
                    [] -> []
                    xs' -> w: wordsWhile p xs''
                     where (w,xs'') = break p xs'


getData :: String ->  IO ([String],[String])
getData fp = readFile fp <&> lines <&> wordsWhile (==[]) <&> takeTwo
  where takeTwo :: [a] -> (a,a)
        takeTwo as = (head as, head $ tail as)


type Rules = M.Map Int [Int] 

createPrior :: [String] -> Rules
createPrior = foldr go M.empty
  where go :: String -> M.Map Int [Int] -> M.Map Int [Int]
        go s m = let s' = wordsWhile (== '|') s
                     a = read $ head s'
                     b = read $ head $ tail s'
                  in
                   M.insertWith (++) b [a] m


createAfter :: [String] -> Rules
createAfter = foldr go M.empty
  where go :: String -> M.Map Int [Int] -> M.Map Int [Int]
        go s m = let s' = wordsWhile (== '|') s
                     a = read $ head s'
                     b = read $ head $ tail s'
                  in
                   M.insertWith (++) a [b] m



-- TODO: rewrite this with fold
verify :: [Int] -> Rules -> Bool
verify [] prior = True
verify (a:as) prior = case M.lookup a prior of
                        Nothing -> verify as prior
                        Just p -> if intersect p as == [] then verify as prior else False


getRelevant :: Int -> [Int] -> Rules -> [Int]
getRelevant x line rule = case M.lookup x rule of
                                  Nothing -> []
                                  Just p -> intersect p line

-- the middle one must have same amount of prior and after
--fixLine :: [Int] -> Rule -> Rule -> Int
fixLine xs prior after =  filter (\x -> go x xs prior after) xs
  where go x xs prior after =  let a = length $ getRelevant x xs prior
                                   b = length $ getRelevant x xs after
                                   n = (length xs)  `mod` 2
                               in
                                 a == b

                        
solve_part1 :: IO ()                        
solve_part1 = do
  (a,b) <- getData "input.txt"
  let p = createPrior a
  let b' = map (map read) (map (wordsWhile (==',')) b)
  let r = filter (\x -> verify x p) b'
  print $ sum $ map (\x -> x !! (length x `div` 2)) r


solve_part2 :: IO ()
solve_part2 = do
  (a,b) <- getData "input.txt"
  let prior = createPrior a
  let after = createAfter a
  let b' = map (map read) (map (wordsWhile (==',')) b)
  let r = filter (\x -> not$ verify x prior) b'
  print $ sum $ (concatMap (\x -> fixLine x prior after) r) 

solve_part1' :: IO ()                        
solve_part1' = do
  (a,b) <- getData "sample.txt"
  let b' = map (map read) (map (wordsWhile (==',')) b)
  let prior = createPrior a
  let after = createAfter a
  print $ (map (\x -> fixLine x prior after) b') 



main = solve_part2
