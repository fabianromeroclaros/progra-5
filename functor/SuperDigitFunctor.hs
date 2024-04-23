import Data.Maybe (Maybe(..))

digitSum :: Integer -> Integer
digitSum 0 = 0
digitSum n = n `mod` 10 + digitSum (n `div` 10)

concatenate :: String -> Int -> Integer
concatenate x n = read $ concat $ replicate n x

superDigit' :: Integer -> Integer
superDigit' n
  | n < 10    = n
  | otherwise = superDigit' $ digitSum n

-- Modificamos la firma de superDigit para que trabaje con Maybe Integer en lugar de Integer
superDigit :: Maybe Integer -> Int -> Maybe Integer
superDigit Nothing _ = Nothing
superDigit (Just x) y = Just $ superDigit' $ concatenate (show x) y

main :: IO ()
main = do
    let input = Just 123
    let repetitions = 3
    let result = superDigit input repetitions
    putStrLn "Resultado:"
    print result
