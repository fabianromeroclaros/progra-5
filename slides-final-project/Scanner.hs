{-# LANGUAGE UndecidableInstances #-}

module Scanner where

import Data.Char (isAlphaNum, isDigit)
import GHC.OldList (isInfixOf)

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

instance (Eq Type) => (Eq Token) where
  (Token String _ _ _) == (Token String _ _ _) = True
  (Token OpenBlock _ _ _) == (Token OpenBlock _ _ _) = True
  (Token EndBlock _ _ _) == (Token EndBlock _ _ _) = True
  (Token Keyword k1 _ _) == (Token Keyword k2 _ _) = k1 == k2
  (Token Error k1 _ _) == (Token Error k2 _ _) = k1 == k2
  (Token EndSlide _ _ _) == (Token EndSlide _ _ _) = True
  (Token t1 s1 _ _) == (Token t2 s2 _ _) = t1 == t2 && s1 == s2

instance Ord Token where
  compare x y
    | x == y = EQ
    | x <= y = LT
    | otherwise = GT
  (Token t1 s1 _ _) <= (Token t2 s2 _ _) = t1 < t2 || (t1 == t2 && s1 <= s2)

scanner :: Input -> [Token]
scanner xs = scan xs 1 1 ""

scan :: Input -> Line -> Col -> String -> [Token]
scan [] _ _ _ = []
scan ('!' : '!' : xs) l c readingCode =
  Token Keyword "!!" l c : scan xs l (c + 2) ""
scan ('_' : '_' : xs) l c readingCode =
  Token Keyword "__" l c : scan xs l (c + 2) (if readingCode == "_" then readingCode else "_")
scan ('*' : '*' : xs) l c readingCode =
  Token Keyword "**" l c : scan xs l (c + 2) (if readingCode == "*" then readingCode else "*")
scan (x : xs) l c readingCode
  | x == '?' =
    let (token, rest) = span (\c -> c `elem` "? Light" || c `elem` "? Dark") (x : xs)
      in Token Keyword token l c : scan rest l (c + length token) ""
  | x == '<' = Token Keyword [x] l c : scan xs l (c + 1) (if readingCode == ">" then readingCode else ">")
  | x == '_' = Token Keyword [x] l c : scan xs l (c + 1) (if readingCode == "_" then readingCode else "_")
  | x == '*' = Token Keyword [x] l c : scan xs l (c + 1) (if readingCode == "*" then readingCode else "*")
  | x == '[' = Token Keyword [x] l c : scan xs l (c + 1) (if readingCode == "]" then readingCode else "]")
  | x == '`' = Token Keyword [x] l c : scan xs l (c + 1) (if readingCode == "`" then readingCode else "`")
  | x `elem` ['!', '>', ']', '?', '|', '$', '%'] = Token Keyword [x] l c : scan xs l (c + 1) ""
  | x == '#' =
    let (token, rest) = span (\c -> c == '#') (x : xs)
      in Token Keyword token l c : scan rest l (c + length token) ""
  | x `elem` [' ', '\t', '\r'] = scan xs l (c + 1) ""
  | x == '\n' = scan xs (l + 1) 1 readingCode
  | x == ';' && readingCode == "" = scan (dropWhile (/= '\n') xs) (l + 1) 1 ""
  | x == '{' = Token OpenBlock [x] l c : scan xs l (c + 1) ""
  | x == '}' = Token EndBlock [x] l c : scan xs l (c + 1) ""
  | x == '-' && head xs == '-' && head (tail xs) == '-' =
      Token EndSlide "---" l c : scan (drop 2 xs) l (c + 2) ""
  | x == '-' = Token Keyword [x] l c : scan xs l (c + 1) ""
  | isStringRead x readingCode =
      let (word, rest) = span (\c -> isStringRead c readingCode) xs
       in Token String (x : word) l c : scan rest l (c + length word + 1) ""
  | otherwise = Token Error [x] l c : scan xs l (c + 1) ""
  where
    isStringRead :: Char -> String -> Bool
    isStringRead x "" = x /= '\n'
    isStringRead x readingCode = [x] /= readingCode
