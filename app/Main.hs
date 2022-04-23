{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Main where

import Hexagram
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B

data Hexagram = Hexagram 
    { num :: Int
    , ptrn :: Int
    , name :: String
    , symb :: String
    , desc :: String
    , descPersonal :: String
    , furtherAspects :: [(Int,String)]
    } deriving (Show, Generic)

instance FromJSON Hexagram
instance ToJSON Hexagram

jsonFile :: FilePath 
jsonFile = "data/22.json"

getJson :: IO B.ByteString 
getJson = B.readFile jsonFile

main = do
    d <- (eitherDecode <$> getJson) :: IO (Either String Hexagram)
    case d of
        Left err -> putStrLn err
        Right ps -> print ps