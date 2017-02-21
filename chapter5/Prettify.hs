module Prettify where

-- JSON document
data Doc = Empty 
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
         deriving (Show, Eq)

empty :: Doc
empty = Empty

text :: String -> Doc
text ""  = Empty
text str = Text str

-- double to doc
double :: Double -> Doc
double = text . show

-- line break
line :: Doc
line = Line

-- concat two Doc values (as ++ for lists)
(<>) :: Doc -> Doc -> Doc
Empty <> y = y
y <> Empty = y
x <> y     = x `Concat` y

-- Char to Doc
char :: Char -> Doc
char = Char

-- concat list of Docs into single one
hcat :: [Doc] -> Doc
hcat = fold (<>)

longcat :: [Doc] -> Doc
longcat [] = empty
longcat ds = line <> fold (\x y -> x <> line <> y) ds

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

-- combine a list of Docs into a single Doc, possibly wrapping lines
fsep :: [Doc] -> Doc
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line           = Char ' '
flatten (x `Union` _)  = flatten x
flatten other          = other

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []     = []
punctuate p [d]    = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

-- compact rendering function
compact :: Doc -> String
compact x = transform [x]
        where transform [] = ""
              transform (d:ds) =
                      case d of
                        Empty        -> transform ds
                        Char c       -> c : transform ds
                        Text s       -> s ++ transform ds
                        Line         -> '\n' : transform ds
                        a `Concat` b -> transform (a:b:ds)
                        _ `Union` b  -> transform (b:ds)

-- pretty rendering
pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
        where best col (d:ds) =
                case d of
                  Empty        -> best col ds
                  Char c       -> c : best (col + 1) ds
                  Text s       -> s ++ best (col + length s) ds
                  Line         -> '\n' : best 0 ds
                  a `Concat` b -> best col (a:b:ds)
                  a `Union` b  -> nicest col (best col (a:ds))
                                             (best col (b:ds))
              best _ _ = ""
              nicest col a b | (width - least) `fits` a = a
                             | otherwise                = b
                             where least = min width col

fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` ""        = True
w `fits` ('\n':_)  = True
w `fits` (c:cs)    = (w - 1) `fits` cs

indented :: Int -> Doc -> String
indented indSize x = transform 0 0 [x]
        where transform offset pendingOffset (d:ds) =
                case d of
                  Empty -> transform offset pendingOffset ds
                  Char c -> let offsets = recalcOffset offset pendingOffset c 
                                offsetBefore = fst offsets
                                offsetAfter = snd offsets
                             in printOffset offsetBefore ++ [c] ++ transform offsetAfter 0 ds
                  Text s -> printOffset pendingOffset ++ s ++ transform offset 0 ds
                  Line -> '\n' : transform offset offset ds
                  a `Concat` b -> transform offset pendingOffset (a:b:ds)
                  a `Union` b -> transform offset 0 (a:ds)
              transform _ _ _ = ""
              recalcOffset current pending c
                | c == '{' || c == '[' = (0, current + indSize)
                | c == '}' || c == ']' = (current - indSize, current - indSize)
                | otherwise            = (pending, current)
              printOffset n = replicate n ' '

