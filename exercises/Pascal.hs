module Pascal where

printPascal :: Int -> [[Int]]
printPascal n = take n pascalTriangle'

pascalTriangle' :: [[Int]]
pascalTriangle' = iterate nextRow [1]

nextRow :: [Int] -> [Int]
nextRow row = zipWith (+) (row ++ [0]) (0 : row)