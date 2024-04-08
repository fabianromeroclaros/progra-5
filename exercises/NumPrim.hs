esPrimo :: Int -> Bool
esPrimo 1 = False
esPrimo n = esPrimoAux n 2

esPrimoAux :: Int -> Int -> Bool
esPrimoAux n divisor
    | divisor * divisor > n = True
    | mod n divisor == 0 = False
    | otherwise = esPrimoAux n (divisor + 1)

numPrimos::Int -> [Int] 
numPrimos n = filter esPrimo [1..n]

solve :: Int -> Int -> Bool
solve n k = any (\comb -> sum comb == n) (combinaciones k (numPrimos n))

combinaciones :: Int -> [a] -> [[a]]
combinaciones 0 _ = [[]]
combinaciones _ [] = []
combinaciones k (x:xs) = map (x :) (combinaciones (k-1) xs) ++ combinaciones k xs