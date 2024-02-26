module Class_02_20_24.Introduccion where

import Data.Char (chr, ord)

-- import Data.Char() para importar todo

suma :: Int -> Int -> Int
suma a b = a + b

suma' :: Float -> Float -> Float
suma' a b = a + b

-- ** es exponencial pero deja con decimales 5.1 ** 10.4 a comparación de ^

{-
    - :t (^)
    - (^) :: (Num a, Integral b) => a -> b -> a
 -}

--  hoogle: Página de buscador de funciones
--  la función round devuelve la parte entera, si el decimal igual menor a 5, redondea al menor, si mayor a 5, al mayor
-- ceil rendondea al superior, floor al inferior, si no tiene decimales (12.00) ambos lo redondean al mismo número

unChar :: Char
unChar = 'A'

-- Para importar o usar un módulo dentro de ghci -> eg: :m Data.Char

tupla :: Char -> (Char, Int)
tupla x = (x, ord x)

sumaTupla :: (Int, Int, Char) -> Int
sumaTupla (a, b, c) = a + b + ord c

sumaLista :: [Int] -> Int
sumaLista xs = sum xs

sumaLista' :: [Int] -> Int
sumaLista' = sum

lista :: [Int]
lista = [1 .. 30]

-- Para tener referencia a la posición de la lista = lista !! 2 -> posición 2 de la lista
-- head, tail functiones para lista, devolver primer elemento y el último
-- take, devuelve una sublista dada un número, es decir si pone 12, devuelve los primeros 12 elementos

esPar :: Int -> Bool
esPar x = even x

filtrarPares :: [Int] -> [Int]
filtrarPares xs = filter esPar xs

-- 1:[] arma lista con ese elemento [1], 1:2:[] = [1,2]