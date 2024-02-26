module NumerosPrimos where

numPrimos::Int -> [Int] 
numPrimos n = [x | x <- [2..n], esPrimo x]
-- Otra forma de hacerlo: 
-- numPrimos n = filter esPrimo [1..n]

esPrimo :: Int -> Bool
esPrimo 1 = False
esPrimo n = esPrimoAux n 2

esPrimoAux :: Int -> Int -> Bool
esPrimoAux n divisor
    | divisor * divisor > n = True
    | mod n divisor == 0 = False
    | otherwise = esPrimoAux n (divisor + 1)