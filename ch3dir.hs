import Data.List
import Data.Ord

data Direction = LeftTurn | Straight | RightTurn deriving (Eq, Show)

data Point a = Point {px :: a, py :: a } deriving Show

turn :: (Ord a, Num a) => Point a -> Point a -> Point a -> Direction
turn a b c = case compare (sign a b c) 0 of EQ -> Straight
                                            GT -> LeftTurn
                                            otherwise -> RightTurn

sign (Point ax ay) (Point bx by) (Point cx cy)
    = (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)

turns (a:b:c:points) = turn a b c : turns (b:c:points)
turns _ = []

cmpPoints :: (Ord a) => Point a -> Point a -> Ordering
cmpPoints (Point ax ay) (Point bx by)
    | ay == by  = compare ax bx
    | otherwise = compare ay by

-- let  in
--  pivot : sortBy (comparing (coTan pivot)) (tail points)
--  where pivot = minimumBy cmpPoints points

coTan pivot point = (px point - px pivot) / (py point - py pivot)
 
graham :: (Ord a, Num a) => [Point a] -> Maybe [Point a]
graham (x:y:points) = Just (func [y, x] points)
    where func fin [] = fin
          func xs@(x:y:finished) (t:todo)
            | not (null finished) && turn y x t == RightTurn = func (y:finished) (t:todo)
            | otherwise                                      = func (t:xs) todo
    
graham _  = Nothing

gscan points = graham (pivot : sortBy (comparing (coTan pivot)) (tail points))
    where pivot = minimumBy cmpPoints points