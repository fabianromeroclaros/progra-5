module Functor where
import Control.Applicative

main = do 
   print (fmap  (+7)(Just 10)) 
   print (fmap  (+7) Nothing)

-- APLICACIÓN A UNA LISTA

-- Definición de una lista
lista :: [Int]
lista = [1, 2, 3, 4, 5]

-- Aplicar la función (+1) a cada elemento de la lista
resultado :: [Int]
resultado = fmap (+1) lista
-- resultado será [2, 3, 4, 5, 6]


-- APLICACIÓN A MAYBE
-- Definición de un valor Maybe
valorMaybe :: Maybe Int
valorMaybe = Just 5

-- Aplicar la función (*2) al valor Maybe
resultadoMaybe :: Maybe Int
resultadoMaybe = fmap (*2) valorMaybe
-- resultadoMaybe será Just 10

-- APLICACIÓN A OTRA FUNCIÓN


-- APLICACIÓN A ARBOL
data Tree a = Leaf a | Node (Tree a) a (Tree a)

instance Functor Tree where
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Node left x right) = Node (fmap f left) (f x) (fmap f right)

-- Crear un árbol binario
arbol :: Tree Integer
arbol = Node (Leaf 1) 2 (Node (Leaf 3) 4 (Leaf 5))

-- Aplicar la función (*2) a cada valor en el árbol
resultadoArbol = fmap (*2) arbol
-- resultadoArbol será Node (Leaf 2) 4 (Node (Leaf 6) 8 (Leaf 10))

-- Aplicando a otra función
ff :: Integer -> Integer
ff = fmap (+3) (+2)
resultadoFf :: Integer
resultadoFf = ff 10