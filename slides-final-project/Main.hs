module Main where

import Scanner(scanner)
import UU.Parsing ( parseIO )
import Parser ( pSlides )
import HTMLGenerator ( generateHTML )

main :: IO ()
main = do input <- readFile "slide.p5"
          let token = scanner input
          putStrLn(show token) 
          tree <- parseIO pSlides token
          putStrLn (show tree)
          putStrLn (generateHTML tree)
          writeFile "presentation/index.html" (generateHTML tree)
