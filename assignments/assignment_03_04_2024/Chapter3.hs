module Chapter3 where

{-
    3. What are the types of the following functions?
 -}
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

{-
    5. Why is it not feasible in general for function types to be instances of the Eq class? When is it feasible? Hint: two functions of the same type are equal if they always return equal results for equal arguments.

    Answer:
    It's difficult to compare functions to see if they're equal because functions can do many different things and work with lots of different values. But if a function always gives the same answer for the same question, then we could compare them.
 -}
