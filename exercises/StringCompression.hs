module StringCompression where

compress :: String -> String
compress [] = []
compress (x:xs) = compress' x 1 xs ""

compress' :: Char -> Int -> String -> String -> String
compress' c count "" result =
    if count == 1 then result ++ [c] else result ++ [c] ++ show count
compress' c count (x:xs) result
    | c == x    = compress' c (count + 1) xs result
    | count == 1 = compress' x 1 xs (result ++ [c])
    | otherwise = compress' x 1 xs (result ++ [c] ++ show count)

