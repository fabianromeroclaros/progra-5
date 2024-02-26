module Class_02_19_24.Example where

nextNum :: (Num a) => a -> a
nextNum n = n + 1

{- Ya que no hay estados, no existe el void -}

square :: (Num a) => a -> a
square n = n * n

suma :: Int -> Int -> Int
suma a b = a + b

cadena :: String
cadena = "Hola mundo"

-- Line comment

esPar :: Int -> Bool
esPar n = mod n 2 == 0;  

esImpar :: Int -> Bool
esImpar n = not (esPar n)

-- Palabra reservada: otherwise
-- :t (||), :t (+)
-- Integer == BigInteger in java, it can occupies what memory stands.

sumaGrande :: Integer -> Integer -> Integer
sumaGrande a b = a + b

-- Fractional :  tipo de dato fraccionales con decimales, es retorno de la división