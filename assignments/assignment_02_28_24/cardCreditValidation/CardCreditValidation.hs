module CardCreditValidation where

toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0    = []
    | otherwise = toDigits (div n 10) ++ [mod n 10]

toDigitsRev ::  Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther ::  [Integer] -> [Integer]
doubleEveryOther = reverse . doubleSecondAux . reverse

doubleSecondAux :: [Integer] -> [Integer]
doubleSecondAux [] = []
doubleSecondAux [x] = [x]
doubleSecondAux (x:y:xs) = x : (y * 2) : doubleSecondAux xs

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

validate ::  Integer -> Bool
validate n = mod (sumDigits (doubleEveryOther (toDigits n))) 10 == 0
