module Class_03_13_2024.DataTypes where

-- Tipos de datos recursivos
data Nat = Zero | Succ Nat deriving (Show, Eq)

-- Guarda un tipo de dato que se pide así mismo, se puede autollamar
-- ghci> Zero
-- ghci> Succ Zero
-- ghci> Succ (Succ Zero)

natToNum :: Nat -> Int
natToNum Zero = 0
natToNum (Succ n) = 1 + natToNum n

-- ghci> natToNum $ Succ (Succ (Succ Zero))
-- 3

numToNat :: Int -> Nat
numToNat 0 = Zero
numToNat n = Succ $ numToNat (n - 1)

-- ghci> numToNat 10
-- Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero)))))))))
-- ghci> natToNum $ numToNat 10
-- 10

suma :: Nat -> Nat -> Nat
suma n m = numToNat (natToNum n + natToNum m)

-- ghci> suma (numToNat 2) (numToNat 3)
-- Succ (Succ (Succ (Succ (Succ Zero))))

-- ghci> natToNum $ suma (numToNat 2) (numToNat 3)
-- 5

suma2 :: Nat -> Nat -> Nat
suma2 Zero n = n
suma2 (Succ m) n = Succ (suma2 m n)

-- ghci> natToNum $ suma2 (numToNat 2) (numToNat 3)
-- 5

-- Representación de árbol:
data Tree a
  = Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Show)
  -- deriving (Show, Eq)

-- Override equals
instance Eq a => Eq (Tree a) where
  Empty == Empty = True
  (Leaf x) == (Leaf y) = x == y
  (Node left1 val1 right1) == (Node left2 val2 right2) =
    (left1 == left2) && (val1 == val2) && (right1 == right2)
  _ == _ = False

-- Representación del arbol:
{-
                7
        5               9
    3       6
-}
arbol :: Tree Int
arbol = Node (Node (Leaf 3) 5 (Leaf 6)) 7 (Leaf 9)

addNode :: (Ord a) => Tree a -> a -> Tree a
addNode Empty x = Leaf x
addNode (Leaf x) y =
  if y <= x
    then Node (Leaf y) x Empty
    else Node Empty x (Leaf y)
addNode (Node left x right) y
  | y <= x = Node (addNode left y) x right
  | otherwise = Node left x (addNode right y)


inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Leaf x) = [x]
inOrder (Node left x right) = inOrder left ++ [x] ++ inOrder right

postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Leaf x) = [x]
postOrder (Node left x right) = postOrder left ++ postOrder right ++ [x]

preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Leaf x) = [x]
preOrder (Node left x right) = [x] ++ preOrder left ++ preOrder right


-- Lista
data List a
  = Vacio
  | Siguiente a (List a)
  deriving (Show)

myNum :: Nat
myNum = Succ (Succ (Zero))

myList :: List String
myList = Siguiente "Manzana" (Siguiente "Limones" Vacio)

-- Siguiente arbol (Siguiente arbol (Vacio))

len :: List a -> Int
len Vacio = 0
len (Siguiente _ xs) = 1 + len xs

rotate :: String -> [String]
rotate str = rotates str (length str)
  where
    rotates _ 0 = []
    rotates strs n = strs : rotates (tail strs ++ [head strs]) (n - 1)


f :: String -> Int -> [String]
f [] _ = []
f (x:xs) n | n > length xs = []
       | otherwise = (xs ++ [x]) : f (xs ++ [x]) (n + 1)
