module ExpressionsV2 where

import Data.Char (isDigit)

data Factor = Lit Integer
    | Negative Factor
    | Parenthesis Expression

data Term = SingleFactor Factor
    | Div Factor Term
    | Mul Factor Term

data Expression = SingleTerm Term
    | Sub Term Expression
    | Add Term Expression

evalExpression :: Expression -> Integer
evalExpression (SingleTerm t) = evalTerm t
evalExpression (Add t e) = evalTerm t + evalExpression e
evalExpression (Sub t e) = evalTerm t - evalExpression e

evalTerm :: Term -> Integer
evalTerm (SingleFactor f) = evalFactor f
evalTerm (Mul f t) = evalFactor f * evalTerm t
evalTerm (Div f t) = evalFactor f `div` evalTerm t

evalFactor :: Factor -> Integer
evalFactor (Lit i) = i
evalFactor (Negative f) = - evalFactor f
evalFactor (Parenthesis e) = evalExpression e

parseExpression :: String -> (Expression,String)
parseExpression s = case op of
    ('+':xs) -> let (e, rest) = parseExpression xs in (Add t e, rest)
    ('-':xs) -> let (e, rest) = parseExpression xs in (Sub t e, rest)
    otherwise -> (SingleTerm t, op)
    where (t, op) = parseTerm s

parseTerm :: String -> (Term, String)
parseTerm s = case op of
    ('*':xs) -> let (t, rest) = parseTerm xs in (Mul f t, rest)
    ('/':xs) -> let (t, rest) = parseTerm xs in (Div f t, rest)
    otherwise -> (SingleFactor f, op)
    where (f, op) = parseFactor s

parseFactor :: String -> (Factor,String)
parseFactor (op:xs) = case op of
    '-' -> let (f,rest) = parseFactor xs in (Negative f, rest)
    '(' -> let (e,')':rest) = parseExpression xs in (Parenthesis e, rest)
    otherwise -> (Lit . read $ takeWhile isDigit (op:xs), dropWhile isDigit (op:xs))

p :: Integer
p = 1000000007;

solve :: String -> Integer
solve xs = evalExpression (fst $ parseExpression $ filter (\x -> x /= ' ') xs) `mod` p;

e0 :: Integer
e0 = solve "22*79-21"

e1 :: Integer
e1 = solve "4/-2/2+8"

e2 :: Integer
e2 = solve "55+3-45*33-25"

e3 :: Integer
e3 = solve "4/-2/(2+8)"
