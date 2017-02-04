module GrahamsAlg(convexHull) where
import TwoD
import Data.List

-- 12. Implement Graham's scan algorithm for the convex hull of a set of 2D points.
convexHull :: [Point2D] -> [Point2D]
convexHull points = 
        let (p1:p2:ps) = sortPoints points (initialPoint points)
         in calculateHull ps [p2, p1]

calculateHull :: [Point2D] -> [Point2D] -> [Point2D]
calculateHull [] hull = hull
calculateHull (p:ps) (h1:h2:hs) = 
        if calcDirection h2 h1 p == DLeft 
           then calculateHull ps (p:h1:h2:hs)
           else calculateHull (p:ps) (h2:hs)

initialPoint :: [Point2D] -> Point2D
initialPoint ps = minimumBy compareCoords ps where
        compareCoords (Point2D x1 y1) (Point2D x2 y2) = 
                let yOrd = compare y1 y2
                 in if yOrd == EQ 
                       then compare x1 x2
                       else yOrd

sortPoints :: [Point2D] -> Point2D -> [Point2D]
sortPoints points initP = sortBy compareAngles points where
        compareAngles p1 p2
          | p1 == initP = LT 
          | p2 == initP = GT
          | otherwise   = compare (slope initP p1) (slope initP p2) where 
                  slope (Point2D x1 y1) (Point2D x2 y2) = (y2 - y1) / (x2 - x1)

