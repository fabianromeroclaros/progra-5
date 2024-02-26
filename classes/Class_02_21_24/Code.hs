module Class_02_21_24.Code where

funciones :: [Integer -> Integer -> Integer]
funciones = [(*), (+)]

{-
    (funciones !! 1) 2 3
    5 (llama a la función del segundo elemento de la lista)
 -}

matriz :: [[Integer]]
matriz = [[1, 3, 4], [1, 3]]

tupla :: (Integer, Integer)
tupla = (1, 3)

-- fst tupla: devuelve el primer valor de la tupla
-- snd tupla: devuelve el segundo valor de la tupla

{-
  zip combina los elementos correspondientes de las listas [1, 2, 3] y ['a', 'b', 'c'] para producir una lista de tuplas [(1,'a'),(2,'b'),(3,'c')].
-}

{-
  takeWhile es una función de orden superior que toma una función de predicado y una lista como argumentos. Retorna la lista de los elementos de la lista de entrada hasta que el primer elemento que no cumple con el predicado es encontrado.

  ghci> takeWhile (< 3) [1, 2, 3, 4, 5]
  [1,2]
-}

{-
  `map` es una función de orden superior que toma una función y una lista como argumentos, y aplica la función a cada elemento de la lista, produciendo una nueva lista con los resultados.
  ghci> map (+1) [1, 2, 3, 4, 5]
  [2, 3, 4, 5, 6]
-}

{-
  drop toma un número entero n y una lista como argumentos, y devuelve una nueva lista que contiene todos los elementos de la lista de entrada, excepto los primeros n elementos.
  ghci> drop 2 [1, 2, 3, 4, 5]
  [3, 4, 5]
-}

{-
  dropWhile es una función de orden superior que toma una función de predicado y una lista como argumentos. Elimina los elementos iniciales de la lista que satisfacen el predicado y devuelve el resto de la lista.

  ghci> dropWhile (< 3) [1, 2, 3, 4, 5]
  [3, 4, 5]
-}

mySplit :: Int -> [a] -> ([a], [a])
mySplit n xs = (take n xs, drop n xs)

myAbs :: Int -> Int
myAbs n = if n >= 0 then n else -n

-- myAbs(-5)

-- Guarded equation
myAbs' :: Int -> Int
myAbs' n
  | n >= 0 = n
  | otherwise = -n

{-
  En Haskell, un "guard" se refiere a una expresión que se utiliza dentro de la definición de una función para evaluar condiciones lógicas y determinar qué acción tomar en función de estas condiciones.

  funcion parametro
    | condicion1 = expresion1
    | condicion2 = expresion2
    | condicionN = expresionN
    | otherwise = expresionPorDefecto
-}

validatePositiveNumber :: Int -> Int
validatePositiveNumber n =
  if n < 0
    then -1
    else if n == 0 then 0 else 1

validatePositiveNumber' :: Int -> Int
validatePositiveNumber' 100 = 100 -- (Pattern matching)
validatePositiveNumber' n
  | n < 0 =
      -1
  | n == 0 = 0
  | otherwise = 1

-- Pattern matching
negar :: Bool -> Bool
negar True = False
negar False = True

esUnoODos :: Int -> String
esUnoODos 1 = "one"
esUnoODos 2 = "two"
esUnoODos _ = "not one either two"

-- underscore _ significa cualquier cosa, es decir cualquier otro valor que no sea ese 1.
