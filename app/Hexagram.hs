{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Hexagram where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import System.Exit
import Data.List
import Data.Maybe
import GHC.Generics
import Numeric

fst' :: (a, b, c) -> a
fst' (x, _, _) = x

snd' :: (a, b, c) -> b
snd' (_, x, _) = x

trd' :: (a, b, c) -> c
trd' (_, _, x) = x

xor :: Char -> Char -> Char
xor a b = if a /= b then '1' else '0'

type Sequence = [Int]

-- King Wen Sequence
wenSeq :: Sequence
wenSeq = [63, 0, 17, 34, 23, 58, 2, 16, 55, 59, 7, 56, 61, 47, 4, 8, 25, 38, 3, 48, 41, 37, 32, 1, 57, 39, 33, 30, 18, 45, 28, 14, 60, 15, 40, 5, 53, 43, 20, 10, 35, 49, 31, 62, 24, 6, 26, 22, 29, 46, 9, 36, 52, 11, 13, 44, 54, 27, 50, 19, 51, 12, 21, 42]

-- Mawangdui Sequence
mawSeq :: Sequence
mawSeq = [63, 56, 60, 59, 58, 61, 57, 62, 36, 39, 32, 35, 34, 37, 33, 38, 18, 23, 16, 20, 19, 21, 17, 22, 9, 15, 8, 12, 11, 10, 13, 14, 0, 7, 4, 3, 2, 5, 1, 6, 27, 31, 24, 28, 26, 29, 25, 30, 45, 47, 40, 44, 43, 42, 41, 46, 54, 55, 48, 52, 51, 50, 53, 49]

-- Gets hexagrams number from binary (--- = 1, - - = 0) reading bottom to top
-- eg. yaoToHexagram 101001 wenSeq == 22
yaoToHexagram :: Sequence -> String -> Int
yaoToHexagram seq b = fromJust (elemIndex (fst . head . readBin . reverse $ b) seq) + 1

valToLine :: Char -> (Char,Char)
valToLine x =
  case x of
    '6' -> ('0','1')
    '7' -> ('1','0')
    '8' -> ('0','0')
    '9' -> ('1','1')

valToHexagram :: Sequence -> Int -> (Int,Int)
valToHexagram seq v = (p,n)
  where
    p = yaoToHexagram seq $ map (fst . valToLine) (show v)
    n = yaoToHexagram seq $ map (uncurry xor . valToLine) (show v)


data Hexagram = Hexagram
  { num :: Int,
    ptrn :: Int,
    name :: String,
    symb :: String,
    desc :: String,
    descPersonal :: String,
    furtherAspects :: [(Int, String)]
  }
  deriving (Show, Generic)

instance FromJSON Hexagram

instance ToJSON Hexagram

readJson :: Int -> IO Hexagram
readJson x = do
  d <- (eitherDecode <$> B.readFile ("data/" ++ show x ++ ".json")) :: IO (Either String Hexagram)
  case d of
    Left err -> die "Hexagram not found"
    Right ps -> return ps