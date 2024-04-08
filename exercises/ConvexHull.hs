module ConvexHull where
import Data.Foldable (maximumBy)
import Data.Function (on)
import Text.Printf (printf)

type Point = (Double, Double)

quicksortByX :: (Ord a) => [(a, a)] -> [(a, a)]
quicksortByX [] = []
quicksortByX (x:xs) = quicksortByX ys ++ [x] ++ quicksortByX zs
    where
        ys = [a | a <- xs, fst a <= fst x]
        zs = [b | b <- xs, fst b > fst x]

getMinX :: [Point] -> Point
getMinX xs = head (quicksortByX xs)

getMaxX :: [Point] -> Point
getMaxX xs = quicksortByX xs !! (length xs - 1)

getSide :: Point -> Point -> Point -> Double
getSide (x, y) (x1, y1) (x2, y2) = cross (x2 - x1, y2 - y1) (x - x1, y - y1)

cross :: (Double, Double) -> (Double, Double) -> Double
cross (x1, y1) (x2, y2) = x1 * y2 - x2 * y1

distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

perimeter :: [Point] -> Double
perimeter ps = sum $ zipWith distance ps (tail ps ++ ps)

distanceToLine :: Point -> Point -> Point -> Double
distanceToLine (x1, y1) (x2, y2) (x, y) =
    let a = y2 - y1
        b = x1 - x2
        c = a * x1 + b * y1
        distance = abs (a * x + b * y - c) / sqrt (a * a + b * b)
    in distance

farthestPointFromLine :: Point -> Point -> [Point] -> Point
farthestPointFromLine p1 p2 points = maximumBy (compare `on` distanceToLine p1 p2) points

isPointInsideTriangle :: Point -> Point -> Point -> Point -> Bool
isPointInsideTriangle (ax, ay) (bx, by) (cx, cy) (dx, dy) =
    let ab = (bx - ax, by - ay)
        ac = (cx - ax, cy - ay)
        ad = (dx - ax, dy - ay)
        det = cross ab ac
        w1 = cross ad ac / det
        w2 = cross ab ad / det
    in w1 >= 0.0 && w2 >= 0.0 && (w1 + w2) <= 1.0

{- quickHull :: [Point] -> [Point]
quickHull [] = []
quickHull points =
    let minX = getMinX points
        maxX = getMaxX points
        farthestPoint = farthestPointFromLine minX maxX points
        leftPoints = [p | p <- points, getSide minX maxX p > 0]
        rightPoints = [p | p <- points, getSide minX maxX p < 0]
        pointsOutsideLeft = filter (\p -> not $ isPointInsideTriangle minX maxX farthestPoint p) leftPoints
        pointsOutsideRight = filter (\p -> not $ isPointInsideTriangle minX maxX farthestPoint p) rightPoints
        hull = quickHull pointsOutsideLeft ++ [minX] ++ quickHull pointsOutsideRight ++ [farthestPoint, maxX]
    in removeDuplicates hull -}

quickHull :: [Point] -> [Point]
quickHull [] = []
quickHull points =
    let minX = getMinX points
        maxX = getMaxX points
        farthestPoint = farthestPointFromLine minX maxX points
        pointsLeft = dividePoints minX maxX farthestPoint points []
        pointsRight = dividePoints farthestPoint maxX minX points []
        hull = quickHull pointsLeft ++ [minX] ++ quickHull pointsRight ++ [farthestPoint, maxX]
    in removeDuplicates hull

dividePoints :: Point -> Point -> Point -> [Point] -> [Point] -> [Point]
dividePoints _ _ _ [] acc = acc
dividePoints p1 p2 p3 (p:ps) acc
    | isPointInsideTriangle p1 p2 p3 p = dividePoints p1 p2 p3 ps acc
    | getSide p1 p3 p > 0 = dividePoints p1 p3 p ps (p:acc)
    | otherwise = dividePoints p3 p2 p ps (p:acc)


removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs)
    | x `elem` xs = removeDuplicates xs
    | otherwise = x : removeDuplicates xs

solve :: [Point] -> Double 
solve points = perimeter (removeDuplicates (quickHull points))


pointsS :: [Point]
pointsS = [(1,1), (2, 5), (3, 3), (5, 3), (3, 2), (2,2)]
