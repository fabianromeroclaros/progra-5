module BinaryClock where

import Control.Monad (replicateM)

-- Convert a 6-digit binary string to an integer
binaryToInt :: String -> Int
binaryToInt = foldl (\acc x -> acc * 2 + read [x]) 0

-- Validate the binary clock representation
validateBinaryClock :: [String] -> String
validateBinaryClock [h1, h2, m1, m2, s1, s2]
  | h >= 0 && h < 24 && m >= 0 && m < 60 && s >= 0 && s < 60 = formatTime h m s
  | otherwise = "ERROR"
  where
    h = binaryToInt (h1 ++ h2)
    m = binaryToInt (m1 ++ m2)
    s = binaryToInt (s1 ++ s2)
    formatTime hh mm ss = show hh ++ ":" ++ show mm ++ ":" ++ show ss

