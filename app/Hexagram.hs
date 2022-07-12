{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Hexagram where

import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import qualified Data.ByteString.Lazy as B
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Text (Text)
import GHC.Generics (Generic)
import Numeric (readBin)
import System.Exit (die)

data Name = Name
  { chinese :: Text,
    pinyin :: Text,
    english :: String
  }
  deriving (Show, Generic, Eq)

data Hexagram = Hexagram
  { number :: Int,
    pattern :: String,
    name :: Name,
    symbol :: Text,
    quote :: Text,
    quoteAuthor :: Text,
    desc :: Text,
    descPersonal :: Text,
    furtherAspects :: [(Int, String)]
  }
  deriving (Show, Generic, Eq)

instance FromJSON Name

instance ToJSON Name

instance FromJSON Hexagram

instance ToJSON Hexagram

type Sequence = [Int]

fst' :: (a, b, c) -> a
fst' (x, _, _) = x

snd' :: (a, b, c) -> b
snd' (_, x, _) = x

trd' :: (a, b, c) -> c
trd' (_, _, x) = x

xor :: Char -> Char -> Char
xor a b = if a /= b then '1' else '0'

-- King Wen Sequence
wenSeq :: Sequence
wenSeq = [63, 0, 17, 34, 23, 58, 2, 16, 55, 59, 7, 56, 61, 47, 4, 8, 25, 38, 3, 48, 41, 37, 32, 1, 57, 39, 33, 30, 18, 45, 28, 14, 60, 15, 40, 5, 53, 43, 20, 10, 35, 49, 31, 62, 24, 6, 26, 22, 29, 46, 9, 36, 52, 11, 13, 44, 54, 27, 50, 19, 51, 12, 21, 42]

-- Mawangdui Sequence
mawSeq :: Sequence
mawSeq = [63, 56, 60, 59, 58, 61, 57, 62, 36, 39, 32, 35, 34, 37, 33, 38, 18, 23, 16, 20, 19, 21, 17, 22, 9, 15, 8, 12, 11, 10, 13, 14, 0, 7, 4, 3, 2, 5, 1, 6, 27, 31, 24, 28, 26, 29, 25, 30, 45, 47, 40, 44, 43, 42, 41, 46, 54, 55, 48, 52, 51, 50, 53, 49]

-- Eight Places Sequence
plaSeq :: Sequence
plaSeq = [63, 62, 60, 56, 48, 32, 40, 47, 9, 8, 10, 14, 6, 22, 30, 25, 18, 19, 17, 21, 29, 13, 5, 2, 36, 37, 39, 35, 43, 59, 51, 52, 0, 1, 3, 7, 15, 31, 23, 16, 54, 55, 53, 49, 57, 41, 33, 38, 45, 44, 46, 42, 34, 50, 58, 61, 27, 26, 24, 28, 20, 4, 12, 11]

-- Binary Sequence (Fu Xi / Shao Yong)
binSeq :: Sequence
binSeq = [0, 32, 16, 48, 8, 40, 24, 56, 4, 36, 20, 52, 12, 44, 28, 60, 2, 34, 18, 50, 10, 42, 26, 58, 6, 38, 22, 54, 14, 46, 30, 62, 1, 33, 17, 49, 9, 41, 25, 57, 5, 37, 21, 53, 13, 45, 29, 61, 3, 35, 19, 51, 11, 43, 27, 59, 7, 39, 23, 55, 15, 47, 31, 63]

emptyHex :: Hexagram
emptyHex = Hexagram {number = 0, pattern = "", name = Name {chinese = "", pinyin = "", english = ""}, symbol = "", quote = "", quoteAuthor = "", desc = "", descPersonal = "", furtherAspects = []}

-- Gets hexagrams number from binary (--- = 1, - - = 0) reading bottom to top
-- eg. yaoToHexagram 101001 wenSeq == 22
yaoToHexagramNum :: Sequence -> String -> Int
yaoToHexagramNum seq b = fromJust (elemIndex (fst . head . readBin . reverse $ b) seq) + 1

valToLine :: Char -> (Char, Char)
valToLine x =
  case x of
    '6' -> ('0', '1')
    '7' -> ('1', '0')
    '8' -> ('0', '0')
    '9' -> ('1', '1')

valToHexagramNum :: Sequence -> String -> (Int, Int)
valToHexagramNum seq v = (p, n)
  where
    p = yaoToHexagramNum seq $ map (fst . valToLine) v
    n = yaoToHexagramNum seq $ map (uncurry xor . valToLine) v

getMovingLn :: String -> String
getMovingLn v = map (snd . valToLine) v

getHexagram :: FilePath -> Int -> IO Hexagram
getHexagram path x = do
  d <- (eitherDecode <$> B.readFile path) :: IO (Either String [Hexagram])
  case d of
    Left err -> die "Hexagram not found"
    Right ps -> return . head $ filter (\a -> number a == x) ps

filterAspects :: String -> Hexagram -> Hexagram
filterAspects s hex = hex {furtherAspects = map snd $ filter (\(a, _) -> a == '1') (zip s $ furtherAspects hex)}

valToHexagram :: Sequence -> FilePath -> String -> (IO Hexagram, IO Hexagram)
valToHexagram seq path v =
  ( pIO,
    do
      p <- pIO
      n <- nIO
      if p == n then return emptyHex else return n
  )
  where
    pIO = do
      p' <- getHexagram path . fst $ valToHexagramNum seq v
      let p = filterAspects (getMovingLn v) p'
      return p
    nIO = do
      n' <- getHexagram path . snd $ valToHexagramNum seq v
      let n = filterAspects "000000" n'
      return n
