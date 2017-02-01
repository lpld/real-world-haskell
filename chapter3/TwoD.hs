module TwoD where

data Direction = DRight 
               | DLeft
               | DStraight
               deriving (Eq, Show)

data Point2D = Point2D Double Double
        deriving (Eq, Show)

-- 9, 10. Write a function that calculates the turn made by two dimensial points
-- and returns a Direction.
calcDirection :: Point2D -> Point2D -> Point2D -> Direction
calcDirection (Point2D x1 y1) (Point2D x2 y2) (Point2D x3 y3) = 
        let (xx1, yy1) = (x2 - x1, y2 - y1) 
            (xx2, yy2) = (x3 - x2, y3 - y2)
         in doCalc (xx1 * yy2 - xx2 * yy1) where 
                doCalc d 
                  | d < 0 = DRight
                  | d > 0 = DLeft 
                  | d == 0 = DStraight

-- 11. Define a function that takes a list of two-dimensional points and 
-- computes the direction of each successive triple.
directions :: [Point2D] -> [Direction]
directions points =
    let toTriples (x1:x2:x3:tail) = (x1, x2, x3) : (toTriples (x2:x3:tail))
        toTriples _ = []
        toDirection (p1, p2, p3) = calcDirection p1 p2 p3
        toDirections (p:ps) = (toDirection p) : (toDirections ps)
        toDirections [] = []
     in toDirections (toTriples points)
