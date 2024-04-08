module MissingNumbers where

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort ys ++ [x] ++ quicksort zs
    where
        ys = [a | a <- xs, a <= x]
        zs = [b | b <- xs, b > x]

delete :: Int -> [Int] -> [Int]
delete _ [] = []
delete x (y:ys) 
    | x == y = ys
    | otherwise = x : delete x ys

deleteRepeated :: [Int] -> [Int] -> [Int]
deleteRepeated [] ys = ys  
deleteRepeated (x:xs) ys = deleteRepeated xs (delete x ys)

findMissingNumbers :: [Int] -> [Int] -> [Int]
findMissingNumbers listA listB =
    quicksort $ deleteRepeated listA listB

result :: [Int]
result = findMissingNumbers [203, 204, 205, 206, 207, 208, 203, 204, 205, 206] [203, 204, 205, 206, 207, 208, 203, 204, 205, 206, 205, 206, 204]