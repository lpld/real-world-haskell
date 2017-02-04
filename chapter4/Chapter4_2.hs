module Chapter4_2 where
import Data.Char

-- 1. Use fold to rewrite and improve upon the asInt function.
-- 2. Extend your function to handle exceptional conditions.
-- 3. Rewrite the function using Either.
type ErrorMessage = String

asInt_fold :: String -> Either ErrorMessage Int
asInt_fold [] = Left "Empty input"
asInt_fold ('-':tail) = fmap negate (asInt_fold tail)
asInt_fold str = foldl handleDigit (Right 0) str
        where handleDigit (Left msg) _ = (Left msg)
              handleDigit (Right acc) x 
                | isDigit x = Right (acc * 10 + digitToInt x)
                | otherwise = Left "Non-digit character"

-- 4. Write your own definiton of the standard concat function
-- using foldr.
concat_foldr :: [[a]] -> [a]
concat_foldr as = foldr (++) [] as

-- 5. Write your own definiton of the standard takeWhile function,
-- first using explicit recursion, and then foldr
takeWhile_rec :: (a -> Bool) -> [a] -> [a]
takeWhile_rec p (x:xs) | p x = x : (takeWhile_rec p xs)
                       | otherwise = []
takeWhile_rec _ [] = []

takeWhile_foldr :: (a -> Bool) -> [a] -> [a]
takeWhile_foldr p as = foldr step [] as
        where step el t | p el = el:t
                        | otherwise = []

-- 6. Write your own implementation of groupBy
groupBy_foldr :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy_foldr eq list = foldr step [] list
        where step el [] = [[el]]
              step el (prev:rest) 
                | eq (head prev) el = (el:prev):rest
                | otherwise = [el]:prev:rest

-- 7. Implement some more Prelude functions: any, cycle, words, unlines
any_fold :: (a -> Bool) -> [a] -> Bool
any_fold p list = foldr check False list
        where check el res = res || p el

cycle_foldr :: [a] -> [a]
cycle_foldr list = foldr (:) (cycle_foldr list) list

words_foldr :: String -> [String]
words_foldr str = foldr step [[]] (dropWhile isSeparator str)
        where step c (prev:rest) 
                | isSeparator c = case prev of
                                    [] -> []:rest
                                    _ -> []:prev:rest
                | otherwise = (c:prev):rest

unlines_foldr :: [String] -> String
unlines_foldr strs = foldr append "" strs
        where append str1 str2 = str1 ++ ('\n':str2)

