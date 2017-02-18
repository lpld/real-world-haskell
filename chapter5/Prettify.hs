module Prettify where

-- JSON document
data Doc = Empty 
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
         deriving (Show, Eq)

text :: String -> Doc
text str = undefined

-- double to doc
double :: Double -> Doc
double num = undefined

-- concat two Doc values (as ++ for lists)
(<>) :: Doc -> Doc -> Doc
a <> b = undefined

-- Char to Doc
char :: Char -> Doc
char c = undefined

-- concat list of Docs into single one
hcat :: [Doc] -> Doc
hcat ds = undefined

-- combine a list of Docs into a single Doc, possibly wrapping lines
fsep :: [Doc] -> Doc
fsep = undefined

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p [] = []
punctuate p [d] = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds
