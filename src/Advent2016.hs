-- Advent of Code 2016 problems in Haskell
{-# LANGUAGE OverloadedStrings #-}
module Advent2016(m1a, m1b, m2a, m2b, m3a, m3b, m4a, m4b, m5a, m5b
                 ) where

import qualified Data.Bifunctor as DBF
import qualified Data.Bits as DBI
import qualified Data.ByteString.Lazy as DBS
import qualified Data.Data as DD
import qualified Data.HashMap.Strict as DHM
import qualified Data.List as DL
import qualified Data.Maybe as DMY
import qualified Data.Scientific as DSC
import qualified Data.String.Utils as DSU
import qualified Data.Text as DT
import qualified Data.Tuple.Utils as DTUU
import qualified Data.Vector as DV
import qualified Data.Word as DW
import qualified Debug.Trace as T

import qualified Data.Hash.MD5 as MD5

import AdventUtils

dataFile filename = "data/2016/" ++ filename

-- Day 1

i1 = readFile $ dataFile "d01.in"

t1a = [("R2, L3",5), ("R2, R2, R2",2), ("R5, L5, R5, R3",12)]

data D1Facing = D1FNorth | D1FEast | D1FSouth | D1FWest
data D1Position = D1Position Int Int D1Facing 

s1RotateLeft :: D1Facing -> D1Facing
s1RotateLeft facing = case facing of
  D1FNorth -> D1FWest
  D1FEast  -> D1FNorth
  D1FSouth -> D1FEast
  D1FWest  -> D1FSouth
  
s1RotateRight :: D1Facing -> D1Facing
s1RotateRight facing = case facing of
  D1FNorth -> D1FEast
  D1FEast  -> D1FSouth
  D1FSouth -> D1FWest
  D1FWest  -> D1FNorth

s1Instructions :: String -> [(Char, Int)]
s1Instructions input = map convert $ words $ traceExpr $ (head $ lines input) ++ ","
                        where convert (x:xs)  = (x, read $ init xs)

s1Rotate :: Char -> D1Position -> D1Position
s1Rotate directive (D1Position x y facing) = case directive of
  'L' ->  D1Position x y (s1RotateLeft facing)
  'R' ->  D1Position x y (s1RotateRight facing)
    
s1Step :: Int -> D1Position -> D1Position
s1Step n (D1Position x y facing) = case facing of
  D1FNorth -> D1Position x (y+n) facing
  D1FEast  -> D1Position (x+n) y facing
  D1FSouth -> D1Position x (y-n) facing
  D1FWest  -> D1Position (x-n) y facing
  
s1Interpret :: D1Position -> (Char,Int) -> D1Position
s1Interpret pos (directive, steps) = s1Step steps $ s1Rotate directive pos

s1AbsPos :: D1Position -> (Int,Int)
s1AbsPos (D1Position x y _) = (x,y)

s1a input = taxicabDistance (s1AbsPos start) $ s1AbsPos $ DL.foldl' s1Interpret start (s1Instructions input)
  where start = D1Position 0 0 D1FNorth

m1a = run s1a t1a i1

t1b = [("R8, R4, R4, R4",4)]

s1InterpretSteps :: D1Position -> (Char,Int) -> [D1Position]
s1InterpretSteps pos (directive, steps) = take steps $ tail $ iterate (s1Step 1) (s1Rotate directive pos)

-- could be made more efficient by dealing with each instruction as line, and finding line crossings
s1b input = taxicabDistance (s1AbsPos start) $ DMY.fromJust $ firstDuplicate positions
  where start = D1Position 0 0 D1FNorth
        positions = concatMap (map s1AbsPos) $ DL.scanl interpreter (return start) (s1Instructions input)
        interpreter moves instruction = s1InterpretSteps (last moves) instruction

m1b = run s1b t1b i1

-- Day 2

i2 = fmap lines $ readFile $ dataFile "d02.in"

t2a = [(["U","L","R","D"],"2125"),(["ULL","RRDDD","LURDL","UUUUD"],"1985")]

s2KeypadMove :: ([Char], [Char], [Char], [Char]) -> Char -> Char -> Char
s2KeypadMove (kpu, kpd, kpr, kpl) position direction = case direction of
  'U' -> translate kpu
  'D' -> translate kpd
  'R' -> translate kpr
  'L' -> translate kpl
  where
    translate newkeypad = newkeypad !! (DMY.fromJust $ DL.elemIndex position "0123456789ABCDEF")

s2KeypadDecode keypad input = tail $ scanl decodeDigit '5' input 
  where
    decodeDigit = foldl $ s2KeypadMove keypad

s2a = s2KeypadDecode (" 123123456", " 456789789", " 233566899", " 112445778")

m2a = run s2a t2a i2

s2b = s2KeypadDecode (" 121452349678B", " 36785ABC9ADCD", " 134467899BCCD", " 122355678AABD")

t2b = [(["ULL","RRDDD","LURDL","UUUUD"],"5DB3")]

m2b = run s2b t2b i2

-- Day 3

i3 = fmap lines $ readFile $ dataFile "d03.in"

t3a = [(["5 10 25"],0)]

s3CheckTriangle :: Int -> Int -> Int -> Bool
s3CheckTriangle a b c = (check a b c) && (check b c a) && (check c a b)
  where
    check a b c = (a + b) > c

s3CountTriangles :: [[Int]] -> Int
s3CountTriangles triangles = length $ filter check triangles
  where
    check :: [Int] -> Bool
    check triangle = s3CheckTriangle (triangle !! 0) (triangle !! 1) (triangle !! 2)

s3a input = s3CountTriangles $ map numberList input

m3a = run s3a t3a i3

t3b = [(["101 301 501","102 302 502","103 303 503","201 401 601","202 402 602","203 403 603"],6)]

s3TripletTranspose :: [[Int]] -> [[Int]]
s3TripletTranspose list = concat $ map DL.transpose $ splitAtAll 3 list

s3b input = s3CountTriangles $ s3TripletTranspose $ map numberList input

m3b = run s3b t3b i3

-- Day 4

i4 = fmap lines $ readFile $ dataFile "d04.in"

t4a = [(["aaaaa-bbb-z-y-x-123[abxyz]","a-b-c-d-e-f-g-h-987[abcde]","not-a-real-room-404[oarel]","totally-real-room-200[decoy]"],1514)]

s4DecodeRoom :: String -> (String,Int,String)
s4DecodeRoom room = (name, sector, checksum)
  where
    (p1,p2) = DL.splitAt (length room - 7) room
    checksum = init $ tail $ p2
    lastSep = last $ DL.elemIndices '-' p1
    (name,secStr) = DL.splitAt lastSep p1
    sector = read $ tail $ secStr

s4NameChecksum name = map head $ take 5 $ DL.sortOn (negate . length) $ DL.group $ DL.sort $ filter (/= '-') name

s4RoomValid (name, sector, checksum) = (s4NameChecksum name) == checksum

s4a input = DL.sum $ map getSector $ filter s4RoomValid $ map s4DecodeRoom input
            where
              getSector (name, sector, checksum) = sector

m4a = run s4a t4a i4

t4b = [(["qzmt-zixmtkozy-ivhz-343[zimth]"],[("very encrypted name",343)])]

s4RotatedAlphabet alphabet n = p2 ++ p1
  where
    (p1,p2) = DL.splitAt (mod n $ length alphabet) alphabet

s4Decrypt n s = map translate s
  where
    translate ch = transTable !! (DMY.fromJust $ DL.elemIndex ch ("-" ++ alphabet))
    transTable = " " ++ s4RotatedAlphabet alphabet n
    alphabet = "abcdefghijklmnopqrstuvwxyz"

s4DecryptRoom (crypticName, sector, checksum) = (s4Decrypt sector crypticName, sector)

s4b input = filter (DL.isInfixOf "north" . fst) $ map s4DecryptRoom $ filter s4RoomValid $ map s4DecodeRoom input

m4b = run s4b [] i4

-- Day 5

i5 = return "uqwqemis"

t5a = [("abc","18f47a30")]

s5MD5 doorid n = MD5.md5s $ MD5.Str (doorid ++ show n)

s5CheckMD5 zeroes md5 = take zeroes md5 == replicate zeroes '0'

s5MD5Codes doorid = filter (s5CheckMD5 5) $ map (s5MD5 doorid) [0..]

s5a input = take 8 $ map (flip (!!) 5) $ s5MD5Codes input

m5a = run s5a t5a i5

t5b = [("abc","05ace8e3")]

s5UpdatePassword :: String -> Char -> Char -> String
s5UpdatePassword password ch p = case DL.elemIndex p ['0'..'7'] of
  Nothing -> password
  Just i  -> if (password !! i) == '-'
             then updateList ch i password
             else password

s5b input = head $ dropWhile (elem '-') $ scanl update "--------" $ s5MD5Codes input
  where
    update password code = traceExpr $ s5UpdatePassword password (code !! 6) (code !! 5)

m5b = run s5b t5b i5
