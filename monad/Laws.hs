module Laws where

-- Ejemplo de identidad izquierda
-- Ley: return x >>= f es lo mismo que f x

-- Maybe Monad
safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing
safeDivide x y = return (x `div` y)

ejemplo1 = return 10 >>= (\x -> safeDivide x 2)  -- Devuelve Just 5
ejemplo2 = (\x -> safeDivide x 2) 10             -- Devuelve Just 5


-- Ejemplo de identidad derecha
-- Ley: m >>= return es lo mismo que m

-- List Monad
listaOriginal :: [Int]
listaOriginal = [1, 2, 3]

ejemplo3 = listaOriginal >>= return  -- Devuelve la lista original
ejemplo4 = listaOriginal            -- Misma lista original


-- Ejemplo de asociatividad
-- Ley: (m >>= f) >>= g es lo mismo que m >>= (\x -> f x >>= g)

-- Maybe Monad
safeDivide' :: Int -> Int -> Maybe Int
safeDivide' _ 0 = Nothing
safeDivide' x y = Just (x `div` y)

ejemplo5 = (Just 10 >>= (\x -> safeDivide' x 2)) >>= (\y -> safeDivide' y 2)  -- Devuelve Just 1
ejemplo6 = Just 10 >>= (\x -> safeDivide' x 2 >>= (\y -> safeDivide' y 2))     -- Devuelve Just 1
