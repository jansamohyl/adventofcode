-- Advent of Code 2015 problems in Haskell
{-# LANGUAGE OverloadedStrings #-}
module Advent2015(m1a, m1b, m2a, m2b, m3a, m3b, m4a, m4b, m5a, m5b, m6a, m6b,
                  m7a, m7b, m8a, m8b, m9a, m9b, m10a, m10b, m11a, m11b, m12a, m12b)
       where

import qualified Data.Bifunctor as DBF
import qualified Data.Bits as DBI
import qualified Data.ByteString.Lazy as DBS
import qualified Data.Data as DD
import qualified Data.HashMap.Strict as DHM
import qualified Data.List as DL
import qualified Data.Maybe as DMY
import qualified Data.Scientific as DSC
import qualified Data.String.Utils as DSU
import qualified Data.Vector as DV
import qualified Data.Word as DW
import qualified Debug.Trace as T

import qualified Data.Aeson as Aeson
import qualified Data.Hash.MD5 as MD5

import AdventUtils

dataFile filename = "data/2015/" ++ filename

-- Day 1

i1 = readFile $ dataFile "d01.in"

t1a = [("(())",0), ("()()",0), ("(((",3), ("(()(()(",3), ("))(((((",3), ("())",-1), ("))(",-1), (")))",-3), (")())())",-3)]

s1a s = countItem '(' s - countItem ')' s

m1a = run s1a t1a i1

t1b = [(")",1), ("()())",5)]

s1bCounter :: Int -> Char -> Int
s1bCounter floor button = case button of
  ')' -> floor-1
  '(' -> floor+1
s1b :: String -> Int
s1b s = length $ takeWhile (/= -1) $ scanl s1bCounter 0 s 

m1b = run s1b t1b i1

-- Day 2

i2 = fmap lines $ readFile $ dataFile "d02.in"

t2a = [(["2x3x4"],58), (["1x1x10"],43)]

s2aParse dimensions = case span (/= 'x') dimensions of
  (dimension, []) -> [read dimension :: Int]
  (dimension, 'x' : dimensions1) -> (read dimension :: Int) : s2aParse dimensions1
s2aWrapping package = 2 * sum areas + minimum areas
                  where areas = map product $ combinations 2 package
s2a packages = sum $ map (s2aWrapping . s2aParse) packages

m2a = run s2a t2a i2

t2b = [(["2x3x4"],34), (["1x1x10"],14)]

s2bRibbon package = minimum perimeters + product package
                    where perimeters = map (\(x:y:_) -> 2*x + 2*y) $ combinations 2 package
s2b packages = sum $ map (s2bRibbon . s2aParse) packages

m2b = run s2b t2b i2

-- Day 3

i3 = readFile $ dataFile "d03.in"

t3a = [(">",2),("^>v<",4),("^v^v^v^v^v",2)]

s3aLocation (x,y) dir = case dir of
  '>' -> (x+1,y)
  '<' -> (x-1,y)
  '^' -> (x,y+1)
  'v' -> (x,y-1)
s3aCoverage instructions = (0,0) : scanl s3aLocation (0,0) instructions
s3a instructions = length $ DL.nub $ s3aCoverage instructions

m3a = run s3a t3a i3

t3b = [("^v",3),("^>v<",3),("^v^v^v^v^v",11)]

s3bSplitInstructions :: String -> (String,String)
s3bSplitInstructions instructions = DBF.bimap (map snd) (map snd) $ DL.partition (fst . DBF.first even) $ enumerate instructions
s3b instructions = length $ DL.nub $ s3aCoverage instructionsSanta ++ s3aCoverage instructionsRobot
                   where (instructionsSanta,instructionsRobot) = s3bSplitInstructions instructions

m3b = run s3b t3b i3

-- Day 4

i4 = return "ckczppom"

t4a = [("abcdef",609043),("pqrstuv",1048970)]

s4aCheckMD5 zeroes secret n = take zeroes md5 == replicate zeroes '0'
  where md5 = MD5.md5s $ MD5.Str (secret ++ show n)
s4aFindBadMD5 zeroes secret = head $ dropWhile (not . s4aCheckMD5 zeroes secret) [1 ..]
s4a = s4aFindBadMD5 5

m4a = run s4a t4a i4

s4b = s4aFindBadMD5 6

m4b = run s4b [] i4

-- Day 5

i5 = fmap lines $ readFile $ dataFile "d05.in"

t5a = [(["ugknbfddgicrmopn"],1),(["aaa"],1),(["jchzalrnumimnmhp"],0),(["haegwjzuvuyypxyu"],0),(["dvszwmarrgswjxmb"],0)]

s5aIsNice word = countVowels word >= 3 && letterTwice && noForbiddenStrings 
                 where countVowels = length . filter (\ch -> elem ch ("aeiou" :: String))
                       letterTwice = any (\(x,y) -> x == y) $ bigrams word
                       noForbiddenStrings = all (\xy -> notElem xy $ zip "acpx" "bdqy") $ bigrams word
s5a words = length $ filter s5aIsNice words 

m5a = run s5a t5a i5

t5b = [(["qjhvhtzxzqqjkmpb"],1),(["xxyxx"],1),(["uurcxstgmygtbstg"],0),(["ieodomkazucvgmuy"],0)]

s5bIsNice word = bigramTwice (bigrams word) && letterTwiceSkipped
                 where bigramTwice [x] = False
                       bigramTwice (x:xs) = (elem x (tail xs)) || bigramTwice xs
                       letterTwiceSkipped = any (\(x,_,y) -> x == y) $ trigrams word
s5b words = length $ filter s5bIsNice words

m5b = run s5b t5b i5

-- Day 6

i6 = fmap lines $ readFile $ dataFile "d06.in"

t6a = [(["turn on 0,0 through 999,999"],1000000),(["toggle 0,0 through 999,0"],1000),(["turn off 499,499 through 500,500"],0)]

s6aOperations word = case word of
          "on" -> const 1 
          "off" -> const 0
          "toggle" -> (-) 1  
s6aParseCommand operationSet cmd = (operation, (x1,x2+1), (y1,y2+1))
  where cmdw = words cmd
        shift = if (cmdw !! 0) == "turn" then 1 else 0
        operation = operationSet $ cmdw !! shift  
        readCoords coords = read ("(" ++ coords ++ ")") :: (Int,Int)
        (x1,y1) = readCoords $ cmdw !! (shift+1)
        (x2,y2) = readCoords $ cmdw !! (shift+3)                         
s6aCommand operationSet board cmd = map replaceRows $ enumerate board
  where (operation,(x1,x2),(y1,y2)) = s6aParseCommand operationSet cmd
        replaceRows (k,row) = if y1 <= k && k < y2 then replaceRow row else row
        replaceRow row = map replaceColumns $ enumerate row
        replaceColumns (k,light) = if x1 <= k && k < x2 then operation light else light
s6aInitBoard = replicate 1000 $ replicate 1000 0
s6aFinalCount board = sum $ map sum board
s6a commands = s6aFinalCount $ foldl (s6aCommand s6aOperations) s6aInitBoard commands

m6a = run s6a t6a i6

s6bOperations word = case word of
          "on" -> (+) 1 
          "off" -> \x -> max 0 (x - 1)
          "toggle" -> (+) 2  
s6b commands = s6aFinalCount $ foldl (s6aCommand s6bOperations) s6aInitBoard commands

m6b = run s6b [] i6

-- Day 7

i7 = fmap (\x -> (x,"a")) $ fmap lines $ readFile $ dataFile "d07.in"

t7aCircuit = ["123 -> x","456 -> y","x AND y -> d","x OR y -> e","x LSHIFT 2 -> f","y RSHIFT 2 -> g","NOT x -> h","NOT y -> i"]
t7a = [((t7aCircuit,"d"),72), ((t7aCircuit,"e"),507), ((t7aCircuit,"f"),492), ((t7aCircuit,"g"),114), ((t7aCircuit,"h"),65412), ((t7aCircuit,"i"),65079), ((t7aCircuit,"x"),123), ((t7aCircuit,"y"),456)]

type D7Word = DW.Word16
data D7LogicElement = D7LEConst D7Word
                    | D7LEVar String
                    | D7LENot String
                    | D7LEAnd String String
                    | D7LEOr String String
                    | D7LEShift String Int
                      deriving (Show)

s7aParseElement description = case words description of
  [v, "->", out] -> case (parse v :: Maybe D7Word) of
    Just vv -> Just (out, D7LEConst vv)
    Nothing -> Just (out, D7LEVar v)
  ["NOT", x, "->", out] -> Just (out, D7LENot x)
  [x, "AND", y, "->", out] -> Just (out, D7LEAnd x y)
  [x, "OR", y, "->", out] -> Just (out, D7LEOr x y)
  [x, "LSHIFT", n, "->", out] -> Just (out, D7LEShift x (read n :: Int))
  [x, "RSHIFT", n, "->", out] -> Just (out, D7LEShift x (negate $ read n :: Int))
  otherwise -> Nothing
  
s7aInterpret ws w = case DHM.lookup w ws of
  Just (D7LEConst v) -> v
  Just (D7LEVar x) -> s7aInterpret ws x
  Just (D7LENot x) -> DBI.complement $ s7aInterpret ws x
  Just (D7LEAnd x y) -> (s7aInterpret ws x) DBI..&. (s7aInterpret ws y)
  Just (D7LEOr x y) -> (s7aInterpret ws x) DBI..|. (s7aInterpret ws y)
  Just (D7LEShift x n) -> DBI.shift (s7aInterpret ws x) n

s7aInterpretFastUnary op ws w w1 = (ws1, v1)
  where (ws0, v0) = s7aInterpretFast ws w1
        v1 = op v0
        ws1 = DHM.insert w (D7LEConst v1) ws0
s7aInterpretFastBinary op ws w w1 w2 = (ws2, v2)
  where (ws0, v0) = s7aInterpretFast ws w1
        (ws1, v1) = s7aInterpretFast ws0 w2
        v2 = op v0 v1
        ws2 = DHM.insert w (D7LEConst v2) ws1
s7aInterpretFast ws w = T.traceShow w $ case DHM.lookup w ws of
  Just (D7LEConst v) -> (ws, v)
  Just (D7LEVar x) -> s7aInterpretFastUnary id ws w x
  Just (D7LENot x) -> s7aInterpretFastUnary DBI.complement ws w x
  Just (D7LEAnd x y) -> s7aInterpretFastBinary (DBI..&.) ws w x y
  Just (D7LEOr x y) -> s7aInterpretFastBinary (DBI..|.) ws w x y
  Just (D7LEShift x n) -> s7aInterpretFastUnary (flip DBI.shift n) ws w x
s7aWires circuit = DHM.fromList $ DMY.mapMaybe s7aParseElement circuit ++ [("0", D7LEConst 0),("1", D7LEConst 1)]
                          
s7a (circuit,wire) = snd $ s7aInterpretFast (s7aWires circuit) wire
                                          
m7a = run s7a t7a i7

s7bCircuitUpdate wires = DHM.insert "b" (D7LEConst 16076) wires
s7b (circuit,wire) = snd $ s7aInterpretFast (s7bCircuitUpdate $ s7aWires circuit) wire

m7b = run s7b [] i7

-- Day 8

i8 = fmap lines $ readFile $ dataFile "d08.in"

t8a = [(["\"\""],2),(["\"abc\""],2),(["\"aaa\\\"aaa\""],3),(["\"\\x27\""],5)]

s8aUnquote s = (read s0 :: String)
  where s0 = DSU.join "\\" $ map fixChars $ DSU.split "\\" s :: String
        fixChars s = case s of
          'x':ch1:ch2:xs -> ("x" ++ [ch1,ch2] ++ "\\&") ++ xs
          otherwise -> s
s8a strings = sum $ map extraSpace strings
              where extraSpace s = length s - (length $ s8aUnquote s)

m8a = run s8a t8a i8

t8b = [(["\"\""],4),(["\"abc\""],4),(["\"aaa\\\"aaa\""],6),(["\"\\x27\""],5)]

s8bQuote s = (show s :: String)
s8b strings = sum $ map extraSpace strings
              where extraSpace s = (length $ s8bQuote s) - length s

m8b = run s8b t8b i8

-- Day 9

i9 = fmap lines $ readFile $ dataFile "d09.in"

t9a = [(["London to Dublin = 464","London to Belfast = 518","Dublin to Belfast = 141"],605)]

s9aParseDist s = case words s of
  [n1, "to", n2, "=", sd] -> [((n1, n2), d), ((n2, n1), d)]
                            where d = read sd :: Int
s9aParseDists distances = concatMap s9aParseDist distances
s9aDistances distances = DHM.fromList $ s9aParseDists distances
s9aCities distances = DL.nub $ map (fst . fst) $ s9aParseDists distances
s9aPathDistances distances = map totalDistance $ DL.permutations (s9aCities distances)
                where totalDistance path = sum $ DMY.mapMaybe (flip DHM.lookup (s9aDistances distances)) $ bigrams path
s9a distances = minimum $ s9aPathDistances distances

m9a = run s9a t9a i9

s9b distances = maximum $ s9aPathDistances distances

m9b = run s9b [] i9

-- Day 10

i10s = "1113222113"
i10a = return (i10s,40)
i10b = return (i10s,50)

t10a = [(("1",1),2),(("11",1),2),(("21",1),4),(("1",5),6)]

s10aRLE s = concatMap (\x -> (show $ length x) ++ [head x]) $ DL.group s
s10a (s,n) = length $ head $ drop n $ iterate s10aRLE s 

m10a = run s10a t10a i10a

m10b = run s10a t10a i10b

-- Day 11

i11 = return "cqjxjnds"

t11a = [("abcdefgh","abcdffaa"),("ghijklmn","ghjaabcc")]

s11a = undefined

m11a = run s11a t11a i11

m11b = run s11a t11a i11

-- Day 12

i12 = DBS.readFile $ dataFile "d12.in"

t12a = [("[1,2,3]",6),("{\"a\":2,\"b\":4}",6),("[[[3]]]",3),("{\"a\":{\"b\":4},\"c\":-1}",3),("{\"a\":[-1,1]}",0),("[-1,{\"a\":1}]",0),("[]",0),("{}",0)]

s12aDecode json = DMY.fromJust (Aeson.decode json :: Maybe Aeson.Value)
s12aSumNumbers :: (Aeson.Object -> Bool) -> Aeson.Value -> DSC.Scientific
s12aSumNumbers objFilter value = case value of
  Aeson.Object obj -> if objFilter obj then DHM.foldl' folder 0 obj else 0
  Aeson.Array arr -> DV.foldl' folder 0 arr
  Aeson.String str -> 0
  Aeson.Number num -> num
  Aeson.Bool b -> 0
  Aeson.Null -> 0
  where folder x y = x + s12aSumNumbers objFilter y
s12a json = s12aSumNumbers (const True) $ s12aDecode json

m12a = run s12a t12a i12

t12b = [("[1,2,3]",6),("[1,{\"c\":\"red\",\"b\":2},3]",4),("{\"d\":\"red\",\"e\":[1,2,3,4],\"f\":5}",0),("[1,\"red\",5]",6)]

s12bNoRedFilter obj = all (Aeson.String "red" /=) $ DHM.elems obj
s12b json = s12aSumNumbers s12bNoRedFilter $ s12aDecode json

m12b = run s12b t12b i12

-- Day 13

