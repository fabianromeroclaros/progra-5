module Chapter6 where

{-
    5. Using the recursive definitions given in this chapter, show how length [1,2,3], drop 3
    [1,2,3,4,5], and init [1,2,3] are evaluated.
 -}

{-
   length [1,2,3]
       = { Aplicamos la definición de length }
       1 + length [2,3]
       = { Aplicamos la definición de length }
       1 + (1 + length [3])
       = { Aplicamos la definición de length }
       1 + (1 + (1 + length []))
       = { Aplicamos la definición de length }
       1 + (1 + (1 + 0))
       = { Sumamos }
       1 + (1 + 1)
       = { Sumamos }
       1 + 2
       = { Sumamos }
       3

   drop 3 [1,2,3,4,5]
       = { Aplicamos la definición de drop }
       drop (3-1) [2,3,4,5]
       = { Aplicamos la definición de drop }
       drop (2-1) [3,4,5]
       = { Aplicamos la definición de drop }
       drop (1-1) [4,5]
       = { Aplicamos la definición de drop }
       drop 0 [4,5]
       = { Aplicamos la definición de drop }
       [4,5]

   init [1,2,3]
       = { Aplicamos la definición de init }
       1 : init [2,3]
       = { Aplicamos la definición de init }
       1 : (2 : init [3])
       = { Aplicamos la definición de init }
       1 : (2 : [])
       = { Listamos }
       [1,2]
   -}

{-
    6. Without looking at the definitions from the standard prelude, define the following library functions
    on lists using recursion.

        a. Decide if all logical values in a list are True:
            and :: [Bool] -> Bool
        b. Concatenate a list of lists:
            concat :: [[a]] -> [a]
        c. Produce a list with n identical elements:
            replicate :: Int -> a -> [a]
        d. Select the nth element of a list:
            (!!) :: [a] -> Int -> a
        e. Decide if a value is an element of a list:
            elem :: Eq a => a -> [a] -> Bool
        Note: most of these functions are defined in the prelude using other library functions rather than
        using explicit recursion, and are generic functions rather than being specific to the type of lists.
 -}

and' :: [Bool] -> Bool
and' [] = True
and' (x : xs) = x && (and' xs)

concat' :: [[a]] -> [a]
concat' [] = []
concat' [x] = x
concat' (x : xs) = x ++ concat' xs

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n - 1) x

(!!) :: [a] -> Int -> a
(!!) (x : _) 0 = x
(!!) (_ : xs) n = (Chapter6.!!) xs (n - 1)

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' e (x : xs) = e == x || elem' e xs

{-
    7. Define a recursive function merge :: Ord a => [a] -> [a] -> [a] that merges two sorted
    lists to give a single sorted list. For example:
    > merge [2,5,6] [1,3,4]
    [1,2,3,4,5,6]
    Note: your definition should not use other functions on sorted lists such as insert or isort, but should be defined using explicit recursion.
 -}
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
  | x <= y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

{-
    8. Using merge, define a function msort :: Ord a => [a] -> [a] that implements merge sort, in
    which the empty list and singleton lists are already sorted, and any other list is sorted by mergingtogether the two lists that result from sorting the two halves of the list separately.
    Hint: first define a function halve :: [a] -> ([a],[a]) that splits a list into two halves whose
    lengths differ by at most one.
 -}
halve :: [a] -> ([a], [a])
halve xs = splitAt (div (length xs)  2) xs

msort :: (Ord a) => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right)
  where
    (left, right) = halve xs

{-
    9. Using the five-step process, construct the library functions that:
        a. calculate the sum of a list of numbers;
        b. take a given number of elements from the start of a list;
        c. select the last element of a non-empty list.
 -}

sumList :: (Num a) => [a] -> a
sumList [] = 0
sumList [x] = x
sumList (x : xs) = x + sumList xs

takeFromStart :: Int -> [a] -> [a]
takeFromStart 0 _ = []
takeFromStart _ [] = []
takeFromStart n (x : xs) = x : takeFromStart (n - 1) xs

selectLast :: [a] -> a
selectLast [x] = x
selectLast (_ : xs) = selectLast xs
