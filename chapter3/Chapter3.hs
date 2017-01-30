import Data.List

-- 1, 2. Write a function that computes the number of elements in a list.
listLength :: [a] -> Int
listLength (_ : xs) = 1 + listLength(xs)
listLength [] = 0

-- 3. Write a function that computes the mean of a list
meanValue :: [Int] -> Float
meanValue (xs) = fromIntegral(listSum xs) / fromIntegral(listLength xs) 
        where listSum [] = 0
              listSum (x : xs) = x + listSum xs

-- 4. Turn a list into a palindrome; i.e., it should read the same both backward and forward.
-- e.g. [1, 2, 3] -> [1, 2, 3, 3, 2, 1]
toPalindrome :: [a] -> [a]
toPalindrome xs = append xs (listReverse xs)
        where append (x:xs) ys = x : append xs ys
              append [] ys = ys

listReverse :: [a] -> [a]
listReverse xs = doReverse xs []
        where doReverse [] result = result
              doReverse (x:xs) result = doReverse xs (x:result)


-- 5. Write a function that determines whether its input list is a palindrome.
isPalindrome :: [Int] -> Bool
isPalindrome xs = listsEqual xs (listReverse xs)
        where listsEqual [] [] = True
              listsEqual [] _ = False
              listsEqual _ [] = False
              listsEqual (x:xs) (y:ys) = x == y && listsEqual xs ys

-- 6. Create a function that sorts a list of lists based on the length of each sublist.
sortByLength :: [[a]] -> [[a]]
sortByLength xs = sortBy compareByLength xs
        where compareByLength xs ys = compare (listLength xs) (listLength ys)

-- 7. Define a function that joins a list of lists together using a separator value.
intersperse :: a -> [[a]] -> [a]
intersperse _ [] = []
intersperse separator (x:xs) = doJoin xs x
        where doJoin [] result = result
              doJoin (x:xs) result = doJoin xs (result ++ (separator : x))

-- 8. Using the binary tree type that we defined earlier, write a function that 
-- will determine the height of the tree.
data Tree a = Node a (Tree a) (Tree a)
            | Empty

treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node _ left right) = max (treeHeight left) (treeHeight right) + 1

-- 9, 10. Write a function that calculates the turn made by two dimensial points
-- and returns a Direction.
data Direction = DRight 
               | DLeft
               | DStraight
               deriving (Eq, Show)

data Point2D = Point2D Double Double
        deriving (Eq, Show)

calcDirection :: Point2D -> Point2D -> Point2D -> Direction
calcDirection (Point2D x1 y1) (Point2D x2 y2) (Point2D x3 y3) = 
        let (xx1, yy1) = (x2 - x1, y2 - y1) 
            (xx2, yy2) = (x3 - x2, y3 - y2)
         in doCalc (xx1 * yy2 - xx2 * yy1) where 
                doCalc d 
                  | d < 0 = DRight
                  | d > 0 = DLeft 
                  | d == 0 = DStraight

