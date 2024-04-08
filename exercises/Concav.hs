module Concav where

getDegrees :: Floating a => (a, a) -> (a, a) -> a
getDegrees x y = convertToDegrees $ acos (getScalarProduct x y / (getMagnitude x * getMagnitude y))

getScalarProduct :: Floating a => (a, a) -> (a, a) -> a
getScalarProduct x y = (fst x * fst y) + (snd x * snd y)

getMagnitude :: Floating a => (a, a) -> a
getMagnitude x = sqrt ((fst x * fst x) + (snd x * snd x))

convertToDegrees :: Floating a => a -> a
convertToDegrees x = x * (180 / pi)

getVector :: Floating a => (a, a) -> (a, a) -> (a, a)
getVector x y = (fst y - fst x, snd y - snd x)

getAngle :: Floating a => (a, a) -> (a, a) -> (a, a) -> a
getAngle x y z = getDegrees (getVector x y) (getVector y z)


isConcav :: (Floating a, Ord a) => [(a, a)] -> Bool
isConcav points
  | length points <= 3 = False
  | otherwise = any (\(x, y, z) -> getAngle x y z > 180) triples
  where
    triples = generateTriples (points ++ [head points] ++ [points !! 1])

generateTriples :: [a] -> [(a, a, a)]
generateTriples [] = []
generateTriples [_] = []
generateTriples [_, _] = []
generateTriples (x:y:z:xs) = (x, y, z) : generateTriples (y:z:xs)

exampleList :: [(Double, Double)]
exampleList = [(0, 0), (0, 1), (1, 1), (1, 0)]


clock (px,py) (qx,qy) (rx,ry) = ((qy - py)*(rx - qx)) - ((qx - px)*(ry - qy))