module Class_03_05_24.Romancal where

roman :: [(String, Int)]
roman =
  [ ("I", 1),
    ("IV", 4),
    ("V", 5),
    ("IX", 9),
    ("X", 10),
    ("XL", 40),
    ("L", 50),
    ("XC", 90),
    ("C", 100),
    ("CD", 400),
    ("D", 500),
    ("CM", 900),
    ("M", 1000)
  ]

romanToNumber :: String -> Int
romanToNumber "" = 0
romanToNumber (x : xs)
  | length xs > 0 && getRoman (x : head xs : []) =
      getNumber (x : head xs : []) + romanToNumber (tail xs)
  | otherwise = getNumber [x] + romanToNumber xs
  where
    getRoman :: String -> Bool
    getRoman x = findNumber x /= []

    findNumber :: String -> [(String, Int)]
    findNumber x = [r | r <- roman, fst r == x]

    getNumber :: String -> Int
    getNumber x = snd $ head $ filter (\n -> fst n == x) roman

numberToRoman :: Int -> String
numberToRoman 0 = ""
numberToRoman num
  | num < 0 = "-" ++ numberToRoman (num * (-1))
  | length (filterRoman num) > 0 = getRoman num
  | otherwise = largestSmallerRoman num
  where
    filterRoman :: Int -> [(String, Int)]
    filterRoman x = filter (\n -> snd n == x) roman

    getRoman :: Int -> String
    getRoman x = fst $ head (filterRoman x)

    largestSmallerRoman :: Int -> String
    largestSmallerRoman num
      | romans == [] = ""
      | otherwise =
        let (r, v) = last romans
        in r ++ numberToRoman (num - v)
      where
        romans :: [(String, Int)]
        romans = filter (\n -> snd n <= num) roman

expr :: [(String, Int -> Int -> Int)]
expr = [("+", (+)), ("-", (-)), ("*", (*)), (":", div), ("%", mod)]


callRoman :: String -> String
callRoman input = input ++ " = " ++ result (words input)
  where
    result :: [String] -> String
    result (x:y:z:xs) = numberToRoman (getOperation y (romanToNumber x) (romanToNumber z))

    getOperation :: String -> Int -> Int -> Int
    getOperation y = snd (head (filter (\n -> fst n == y) expr))

