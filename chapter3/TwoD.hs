module TwoD where

data Direction = DRight 
               | DLeft
               | DStraight
               deriving (Eq, Show)

data Point2D = Point2D Double Double
        deriving (Eq, Show)

