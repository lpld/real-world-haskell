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
toPalindrome xs = append xs (reverse xs [])
    where reverse [] result = result
          reverse (x:xs) result = reverse xs (x:result)
          append (x:xs) ys = x : append xs ys
          append [] ys = ys

-- 5. Write a function that determines whether its input list is a palindrome.
-- isPalindrome :: [a] -> Bool
