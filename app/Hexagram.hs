module Hexagram where

import Data.List
import Data.Maybe
import Numeric

fst' :: (a,b,c) -> a
fst' (x,_,_) = x

snd' :: (a,b,c) -> b
snd' (_,x,_) = x

trd' :: (a,b,c) -> c
trd' (_,_,x) = x

type Sequence = [Int]

-- King Wen Sequence
wenSeq :: Sequence
wenSeq = [63, 0, 17, 34, 23, 58, 2, 16, 55, 59, 7, 56, 61, 47, 4, 8, 25, 38, 3, 48, 41, 37, 32, 1, 57, 39, 33, 30, 18, 45, 28, 14, 60, 15, 40, 5, 53, 43, 20, 10, 35, 49, 31, 62, 24, 6, 26, 22, 29, 46, 9, 36, 52, 11, 13, 44, 54, 27, 50, 19, 51, 12, 21, 42]

-- Mawangdui Sequence
mawSeq :: Sequence
mawSeq = [63, 56, 60, 59, 58, 61, 57, 62, 36, 39, 32, 35, 34, 37, 33, 38, 18, 23, 16, 20, 19, 21, 17, 22, 9, 15, 8, 12, 11, 10, 13, 14, 0, 7, 4, 3, 2, 5, 1, 6, 27, 31, 24, 28, 26, 29, 25, 30, 45, 47, 40, 44, 43, 42, 41, 46, 54, 55, 48, 52, 51, 50, 53, 49]

-- Gets hexagrams number from binary (--- = 1, - - = 0) reading bottom to top
-- eg. yaoToHexagram 101001 wenSeq == 22
yaoToHexagram :: Int -> Sequence -> Int
yaoToHexagram b seq = fromJust (elemIndex (fst . head . readBin . reverse . show $ b) seq) + 1

-- Gets hexagrams using line value (--- = 7, - - = 8, -⦵- = 9, -X- = 6)
-- (furtherAspects,current,future)
-- eg. valToHexagram 789776 wenSeq == ({00}1001,49,25)
valToHexagram :: Int -> Sequence -> (Int,Int,Int)
valToHexagram v seq = (read f, yaoToHexagram (read p) seq, yaoToHexagram (read n) seq )
    where
        toBin x = case x of
                    '7' -> '1'
                    '8' -> '0'
        p = map (\x -> if x == '9' then '1' else if x == '6' then '0' else toBin x ) (show v)
        n = map (\x -> if x == '9' then '0' else if x == '6' then '1' else toBin x ) (show v)
        f = map (\x -> if x == '9' || x == '6' then '1' else '0') (show v)
        
