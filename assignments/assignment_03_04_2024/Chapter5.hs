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