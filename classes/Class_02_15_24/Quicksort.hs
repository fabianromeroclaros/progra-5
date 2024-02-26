f :: (Ord a) => [a] -> [a]
f [] = []
f (x:xs) = f ys ++ [x] ++ f zs
    where
        ys = [a | a <- xs, a <= x]
        zs = [b | b <- xs, b > x]

main = do
    let unsortedList = [12, 7, 11, 3, 9, 2, 6]
    putStrLn "Unsorted list:"
    print unsortedList
    putStrLn "Sorted list:"
    print (f unsortedList)
