{-# LANGUAGE UndecidableInstances #-}

module Scanner where
import Data.Char (isAlphaNum)

-- cabal install uulib

type Col = Int

type Line = Int

type Value = String

type Input = String

data Token = Token Type Value Line Col

data Type
  = String
  | OpenBlock
  | EndBlock
  | Keyword
  | EndSlide
  | Error
  | Comment
  deriving (Eq, Ord)

instance Show Token where
  show (Token t v l c) = show t ++ show v ++ " " ++ show l ++ " " ++ show c ++ "\n"

instance Show Type where
  show String = "String: "
  show OpenBlock = "OpenBlock: "
  show EndBlock = "EndBlock: "
  show Keyword = "Keyword: "
  show Error = "Error: "
  show EndSlide = "EndSlide: "
  -- show Comment = "Comment: "

instance (Eq Type) => (Eq Token) where
  (Token String s1 _ _) == (Token String s2 _ _) = True
  (Token OpenBlock _ _ _) == (Token OpenBlock _ _ _) = True
  (Token EndBlock _ _ _) == (Token EndBlock _ _ _) = True
  (Token Keyword _ _ _) == (Token Keyword _ _ _) = True
  (Token Error _ _ _) == (Token Error _ _ _) = True
  (Token EndSlide _ _ _) == (Token EndSlide _ _ _) = True
  (Token t1 s1 _ _) == (Token t2 s2 _ _) = t1 == t2 && s1 == s2

instance Ord Token where
  compare x y | x == y = EQ
              | x <= y = LT
              | otherwise = GT
  (Token t1 s1 _ _) <= (Token t2 s2 _ _) = t1 < t2 || (t1 == t2 && s1 <= s2)


scanner :: Input -> [Token]
scanner xs = scan xs 1 1

scan :: Input -> Line -> Col -> [Token]
scan [] _ _ = []
scan (x : xs) l c
  | x == '!' = Token Keyword [x] l c : scan xs l (c + 1)
  | x == '#' = Token Keyword [x] l c : scan xs l (c + 1)
  | x == ' ' = scan xs l (c + 1)
  | x == '\n' = scan xs (l + 1) 1
  | x == ';' = scan (dropWhile (/= '\n') xs) (l + 1) 1
  | x == '{' = Token OpenBlock [x] l c : scan xs l (c + 1)
  | x == '}' = Token EndBlock [x] l c : scan xs l (c + 1)
  | x == '-' && head xs == '-' && head (tail xs) == '-' = Token EndSlide "--" l c : scan (drop 2 xs) l (c + 2)
  | isAlphaNum x = let (word, rest) = span isAlphaNumOrSpace xs
                   in Token String (x:word) l c : scan rest l (c + length word + 1)
  | otherwise = Token Error [x] l c : scan xs l (c + 1)
  where isAlphaNumOrSpace = (`elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' ']))

