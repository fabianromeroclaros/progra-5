module Chapter7 where

import Data.Char (chr, ord)

{-
    6. A higher-order function unfold that encapsulates a simple pattern of recursion for producing a list can be defined as follows:
    That is, the function unfold p h t produces the empty list if the predicate p is true of the
    argument value, and otherwise produces a non-empty list by applying the function h to this value to give the head, and the function t to generate another argument that is recursively processed in the same way to produce the tail of the list. For example, the function int2bin can be rewritten more compactly using unfold as follows:
        int2bin = unfold (== 0) (‘mod‘ 2) (‘div‘ 2)
    Redefine the functions chop8, map f and iterate f using unfold.
 -}

type Bit = Int

unfold :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

int2bin :: Int -> [Bit]
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold (== []) (take 8) (drop 8)

map' :: (Eq a) => (a -> b) -> [a] -> [b]
map' f = unfold (== []) (f . head) tail

iterate' :: (a -> a) -> a -> [a]
iterate' = unfold (const False) id

{-
    7. Modify the binary string transmitter example to detect simple transmission errors using the concept of parity bits. That is, each eight-bit binary number produced during encoding is extended with a parity bit, set to one if the number contains an odd number of ones, and to zero otherwise. In turn, each resulting nine-bit binary number consumed during decoding is checked to ensure that its parity bit is correct, with the parity bit being discarded if this is the case, and a parity error being reported otherwise.
    Hint: the library function error :: String -> a displays the given string as an error message
    and terminates the program; the polymorphic result type ensures that error can be used in any
    context.
 -}

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (addparity . make8 . int2bin . ord)

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0

parity :: [Bit] -> Bit
parity bs
  | odd (sum bs) = 1
  | otherwise = 0

checkparity :: [Bit] -> [Bit]
checkparity (b : bs)
  | b == parity bs = bs
  | otherwise = error "parity mismatch"

decode :: [Bit] -> String
decode = map (chr . bin2int . checkparity) . chop9

addparity :: [Bit] -> [Bit]
addparity bs = (parity bs) : bs

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

channel :: [Bit] -> [Bit]
channel = id

transmit :: String -> String
transmit = decode . channel . encode

{-
    Define a function altMap :: (a -> b) -> (a -> b) -> [a] -> [b] that alternately applies
    its two argument functions to successive elements in a list, in turn about order. For example:
    > altMap (+10) (+100) [0,1,2,3,4]
    [10,101,12,103,14]
 -}

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f0 f1 (x : xs) = f0 x : altMap f1 f0 xs
