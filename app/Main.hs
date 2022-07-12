module Main where

import Hexagram

main = do
    let (a,b) = valToHexagram wenSeq "data/Hexagrams.json" "787887"
    x <- a
    y <- b
    print x
    putStrLn "\nNext\n"
    print y