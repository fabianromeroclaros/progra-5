{- -- Función para dividir dos números
divideMaybe :: Integer -> Integer -> Maybe Integer
divideMaybe _ 0 = Nothing
divideMaybe x y = return (x `div` y)

-- Ejemplo de uso
main :: IO ()
main = do
    let resultado1 = divideMaybe 10 2   -- Devuelve Just 5
    let resultado2 = divideMaybe 10 0   -- Devuelve Nothing (división por cero)

    -- Imprimir los resultados
    putStrLn $ "Resultado 1: " ++ show resultado1
    putStrLn $ "Resultado 2: " ++ show resultado2

 -}


-- Funciones que pueden fallar y devolver Nothing
{- divideMaybe :: Int -> Int -> Maybe Int
divideMaybe _ 0 = Nothing
divideMaybe x y = Just (x `div` y)

subtractMaybe :: Int -> Int -> Maybe Int
subtractMaybe x y
    | x < y     = Nothing
    | otherwise = Just (x - y)

-- Función para encadenar las operaciones utilizando >>=
calculate :: Int -> Int -> Maybe Int
calculate x y = divideMaybe x y >>= (\result -> subtractMaybe result 2)

main :: IO ()
main = do
    let result1 = calculate 10 2  -- Devuelve Just 3
    let result2 = calculate 8 4   -- Devuelve Just 0
    let result3 = calculate 6 0   -- Devuelve Nothing (división por cero)

    -- Imprimir los resultados
    putStrLn $ "Resultado 1: " ++ show result1
    putStrLn $ "Resultado 2: " ++ show result2
    putStrLn $ "Resultado 3: " ++ show result3
 -}

{- main :: IO ()
main = do
    putStrLn "Hello," >> putStrLn "world!"
     -}