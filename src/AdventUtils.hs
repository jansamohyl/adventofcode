-- Advent of Code - utilities
{-# LANGUAGE OverloadedStrings #-}
module AdventUtils(isEven, returnList, countItem, splitAtAll, combinations,
                   enumerate, bigrams, trigrams, tup2List, parse, traceExpr,
                   runTest, runTests, run
                  ) where

import qualified Debug.Trace as T

isEven :: Int -> Bool
isEven x = mod x 2 == 0

returnList :: a -> [a]
returnList = return

countItem :: Eq a => a -> [a] -> Int
countItem x xs = length $ filter (x ==) xs

splitAtAll :: Int -> [a] -> [[a]]
splitAtAll n [] = []
splitAtAll n list = x : splitAtAll n xs
                    where (x,xs) = splitAt n list

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = [ x:y | y <- combinations (n-1) xs ] ++ combinations n xs

enumerate :: [a] -> [(Int,a)]
enumerate xs = tail $ scanl (\(i,_) -> \el -> (i+1,el)) (-1,undefined) xs

bigrams :: [a] -> [(a,a)]
bigrams s = zip s (tail s)

trigrams :: [a] -> [(a,a,a)]
trigrams s = zip3 s (tail s) (tail $ tail s)

tup2List :: (a,a) -> [a]
tup2List (a1,a2) = [a1,a2]

parse :: Read a => String -> Maybe a
parse s = case (reads s :: Read a => [(a,String)]) of
  [] -> Nothing
  [(x,ss)] -> Just x

traceExpr :: Show a => a -> a 
traceExpr x = T.traceShow x x

-- Very simple test & run framework

runTest :: (Show a, Show b, Eq b) => (a->b) -> (a,b) -> Either String String
runTest f (i,o) = if r == o
                  then Right ("Test " ++ (show i) ++ " -> " ++ (show o) ++ " OK")
                  else Left ("Test " ++ (show i) ++ " -> " ++ (show o) ++ " FAIL: " ++ (show r))
                  where r = f i

runTests :: (Show a, Show b, Eq b) => (a->b) -> [(a,b)] -> Either String [String]
runTests f suite = sequence tests
  where tests = map (runTest f) suite :: [Either String String]

run :: (Show a, Show b, Eq b) => (a->b) -> [(a,b)] -> IO a -> IO ()
run pgm tests input = case runTests pgm tests of
  Left failedTest -> putStrLn failedTest
  Right passedTests -> do
    putStrLn $ unlines passedTests
    i <- input
    putStrLn $ "RESULT: " ++ show (pgm i)