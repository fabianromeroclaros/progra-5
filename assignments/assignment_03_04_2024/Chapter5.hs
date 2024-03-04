module Chapter5 where

{-
    6. A positive integer is perfect if it equals the sum of all of its factors, excluding the number itself.
    Using a list comprehension and the function factors, define a function perfects :: Int ->
    [Int] that returns the list of all perfect numbers up to a given limit. For example:
    > perfects 500
    [6,28,496]
-}

factors :: Int -> [Int]
factors n = [x | x <- [1 .. (div n 2)], mod n x == 0]

perfects :: Int -> [Int]
perfects limit = [x | x <- [1 .. limit], sum (factors x) == x]

{-
    7. Show how the list comprehension [(x,y) | x <- [1,2], y <- [3,4]] with two generators
    can be re-expressed using two comprehensions with single generators. Hint: nest one
    comprehension within the other and make use of the library function concat :: [[a]] -> [a].
-}
reexpressedList :: [(Integer, Integer)]
reexpressedList = concat [[(x, y) | y <- [3, 4]] | x <- [1, 2]]

{-
    8. Redefine the function positions using the function find.

    positions :: Eq a => a -> [a] -> [Int]
    positions x xs = [i | (x’,i) <- zip xs [0..], x == x’]

    find :: Eq a => a -> [(a,b)] -> [b]
    find k t = [v | (k’,v) <- t, k == k’]
 -}

find :: (Eq a) => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions :: (Eq a) => a -> [a] -> [Int]
positions x xs = find x (zip xs [0 ..])

{-
    9. The scalar product of two lists of integers xs and ys of length n is given by the sum of the products
    of corresponding integers:
    In a similar manner to chisqr, show how a list comprehension can be used to define a function
    scalarproduct :: [Int] -> [Int] -> Int that returns the scalar product of two lists. For
    example:
    > scalarproduct [1,2,3] [4,5,6]
    32
 -}
scalarproduct :: (Num a) => [a] -> [a] -> a
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]
