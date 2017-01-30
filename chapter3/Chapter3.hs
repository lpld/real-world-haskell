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


