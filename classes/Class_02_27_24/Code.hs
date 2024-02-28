-- foldr (*) 1 [1..5]
{-
    La función foldr en Haskell es una función de orden superior que se utiliza para reducir una lista a un solo valor aplicando una función binaria a cada elemento de la lista, comenzando desde el final de la lista
-}

-- * - funcion, 1 valor inicial, lista

{-
    Argumentos:
    Una función binaria que toma dos argumentos: el primer argumento es un elemento de la lista y el segundo argumento es el acumulador.
    Un valor inicial para el acumulador.
    Una lista sobre la cual se va a realizar el plegado.
-}
-- 120

mySum :: Num a => [a] -> a
mySum = foldr (+) 0
-- Por currying, la anterior línea es igual a:
-- mySum xs = foldr (+) 0 xs

myFac :: Num a => [a] -> a
myFac = foldr (*) 1

{- 
    zipWith es una función de orden superior en Haskell que toma una función binaria y dos listas como argumentos y devuelve una lista que resulta de aplicar la función a los elementos correspondientes de las dos listas de entrada
-}

{- 
    zipWith (+) [1, 2, 3] [4, 5, 6]
    Resultado: [5, 7, 9]
-}

{- 
    foldr: 1 + (2 + (3 + 0))
    La diferencia es la forma de evaluación, en r, evalúa primero el 3 + 0 y así sucesivamente, en l primero el 0 + 1
    foldl: ((0 + 1) + 2) + 3
-}

square :: Int -> Int
square x = x * x

{- 
    l operador . (punto) se utiliza para componer funciones. La composición de funciones es una técnica que se utiliza para combinar dos o más funciones en una nueva función.

    La expresión f . g se lee como "f después de g" o "f de g". Esto significa que la salida de g se pasa como entrada a f. La función resultante de esta composición es una nueva función que realiza primero la operación de g y luego la operación de f sobre el resultado de g.
-}

-- (.) Función de composición
twiceSquare :: Int -> Int
twiceSquare = square . square
-- twiceSquare x = square(square x)

showNum :: Int -> String
showNum = show . square