digitSum :: Integer -> Integer
digitSum 0 = 0
digitSum n = n `mod` 10 + digitSum (n `div` 10)

concatenate :: String -> Int -> Integer
concatenate x n = read $ concat $ replicate (fromIntegral n) x

superDigit' :: Integer -> Integer
superDigit' n
  | n < 10    = n
  | otherwise = superDigit' $ digitSum n

superDigit :: Integer -> Int -> Integer
superDigit x y = superDigit' $ concatenate (show x) y
