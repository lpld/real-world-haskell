module Chapter4 where

-- 1. Write your own "safe" definitions of the standard partial list functions.
safeHead :: [a] -> Maybe a
safeHead xs = safeListFun xs head

safeTail :: [a] -> Maybe [a]
safeTail xs = safeListFun xs tail

safeLast :: [a] -> Maybe a
safeLast xs = safeListFun xs last

safeInit :: [a] -> Maybe [a]
safeInit xs = safeListFun xs init

safeListFun :: [a] -> ([a] -> b) -> Maybe b
safeListFun [] _ = Nothing
safeListFun xs fun = Just (fun xs)

-- 2. Write function splitWith that takes a predicate and a list of any type, and the splits its 
-- input list on every element for which the predicate returns False.
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith pred as =
        let notPred a = not (pred a)
            trimmed = dropWhile notPred as
            (word, rest) = span pred trimmed
         in case word of
              [] -> []
              ws -> ws : splitWith pred rest
