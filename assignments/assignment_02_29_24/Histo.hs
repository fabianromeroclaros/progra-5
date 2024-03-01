module Histo where

frecuencia :: [Int] -> [Int]
frecuencia xs = [length (filter (\x -> x == n) xs) | n <- [0..9]]

-- Devolver empezando del máximo 
-- Contador, del maximo -1, si el maximo es 4, un array de strings
-- La primera fila con la condicional n <= contador devuelve * sino un " "
filaString :: Int -> [Int] -> String
filaString n xs = [if f >= n then '*' else ' ' | f <- xs]


generarFilas :: [Int] -> [String]
generarFilas xs = generarFilasAux (maximum xs) xs []

generarFilasAux :: Int -> [Int] -> [String] -> [String]
generarFilasAux contador xs ys 
    | contador == 0 = ys
    | otherwise = ys ++ [filaString contador xs] ++ generarFilasAux (contador - 1) xs ys

saltosString :: [String] -> String
saltosString [] = ""
saltosString (x:xs) = x ++ "\n" ++ saltosString xs

obtenerStringHistograma :: [Int] -> String
obtenerStringHistograma xs = saltosString(generarFilas(frecuencia xs)) ++ 
    ['=' | x <- [0..9]] ++ "\n0123456789"

histograma :: [Int] -> IO ()
histograma xs = putStrLn(obtenerStringHistograma xs)