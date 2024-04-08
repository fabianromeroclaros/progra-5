import Data.List (nub)


-- update list

updateAbsolute :: [Int] -> [Int]
updateAbsolute = map abs

-- Pascal's triangle

pascalTriangle :: Int -> [[Int]]
pascalTriangle n = take n $ iterate nextRow [1]
    where
        nextRow row = zipWith (+) (row ++ [0]) (0 : row)

-- Mingle String

mingledString :: String -> String -> String
mingledString [] [] = []
mingledString (x:xs) (y:ys) = x : y : mingledString xs ys


-- string o permute
swapCharacters :: String -> String
swapCharacters [] = []
swapCharacters [x] = [x]
swapCharacters (x:y:xs) = y : x : swapCharacters xs

-- convex Hull

-- String comprension 

compressMessage :: String -> String
compressMessage [] = []
compressMessage [x] = [x]
compressMessage (x:xs) = compressed x 1 xs
  where
    compressed c count [] = if count == 1 then [c] else c : show count
    compressed c count (y:ys)
      | c == y    = compressed c (count + 1) ys
      | count == 1 = c : compressed y 1 ys
      | otherwise = show count ++ compressed y 1 ys

-- super digit
-- Función para calcular el super dígito de un número
superDigit :: Integer -> Integer
superDigit n
    | n < 10    = n
    | otherwise = superDigit $ sumDigits n

-- Función para sumar los dígitos de un número
sumDigits :: Integer -> Integer
sumDigits 0 = 0
sumDigits n = n `mod` 10 + sumDigits (n `div` 10)

-- Función para calcular el super dígito de un número repetido k veces
repeatedSuperDigit :: Integer -> Integer -> Integer
repeatedSuperDigit n k = superDigit (n * k)


-- Rotate String
rotateLeft :: String -> String
rotateLeft [] = []
rotateLeft (x:xs) = xs ++ [x]

-- Función para generar todas las rotaciones de una cadena
rotations :: String -> [String]
rotations str = take (length str) (iterate rotateLeft str)

-- Remove Duplicates

isIn :: Char -> String -> Bool
isIn _ [] = False
isIn c (x:xs)
    | c == x    = True
    | otherwise = isIn c xs

removeDuplicates :: String -> String
removeDuplicates [] = []
removeDuplicates (x:xs)
    | elem x xs = removeDuplicates xs  -- Si x está presente en xs, lo ignoramos
    | otherwise   = x : removeDuplicates xs  -- Si x no está presente en xs, lo mantenemos
 

removeDuplicates2 :: String -> String
removeDuplicates2 = nub

-- Pascal triangle
-- printPascal :: [Int] -> [Int]
-- printPascal xs = [1] ++ row xs

printPascal :: Int -> [[Int]]
printPascal n = take n pascalTriangle'

pascalTriangle' :: [[Int]]
pascalTriangle' = iterate nextRow [1]

nextRow :: [Int] -> [Int]
nextRow row = zipWith (+) (row ++ [0]) (0 : row)

row :: [Int] -> [Int]
row [] = []
row [x] = [x]
row (x:y:xs) = (x+y) : row (y:xs)

-- map (\x -> folr (++) "" (map show x)) [[1], [1,1], [1,2,1], [1,3,3,1]]