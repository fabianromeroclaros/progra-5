module Main where

import Scanner(scanner)
import UU.Parsing
import Parser

main :: IO ()
main = do input <- readFile "slide.p5"
          let token = scanner input
          putStrLn(show token) 
          tree <- parseIO pSlides token
          putStrLn (show tree)
