module Chapter3 where

-- Exercises 3.11
-- 1. What are the types of the following values?
-- ['a','b','c'] :: [Char]
-- ('a','b','c') :: (Char,Char,Char)
-- [(False,'O'),(True,'1')] :: [(Bool,Char)]
-- ([False,True],['0','1']) :: ([Bool],[Char])
-- [tail, init, reverse] :: [[a] -> [a]]

-- 2. Write down definitions that have the following types; it does not matter what the definitions actually do as long as they are type correct.

bools :: [Bool]
bools = [True, True, False]

nums :: [[Int]]
nums = [[1, 2, 3, 4], [4, 3, 2, 1], [5, 6, 7, 8], [9, 10, 11, 12]]

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a, a)
copy x = (x, x)

apply :: (a -> b) -> a -> b
apply f x = f x


-- 3. What are the types of the following functions?
second :: [a] -> a
second xs = head (tail xs)

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

pair :: a -> b -> (a, b)
pair x y = (x, y)

double :: (Num a) => a -> a
double x = x * 2

palindrome :: (Eq a) => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- Why is it not feasible in general for function types to be instances of the Eq class? When is it feasible? Hint: two functions of the same type are equal if they always return equal results for equal arguments.

-- Answer: 
-- It's difficult to compare functions to see if they're equal because functions can do many different things and work with lots of different values. But if a function always gives the same answer for the same question, then we could compare them.
