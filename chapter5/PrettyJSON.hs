module PrettyJSON (renderJValue, renderJValueLong) where

import Prettify
import SimpleJSON
import Numeric (showHex)
import Data.Bits (shiftR, (.&.))
import Data.Char (ord)

-- function that combines a list of values ([a]) into a single Doc,
-- putting commas between them and enclosing the result.
-- Char -> Char : characters to enclose the result in
-- (a -> Doc)   : function to convert a single 'a' into a Doc
-- [a]          : a list of values 'a'
type SeriesFunction a = Char -> Char -> (a -> Doc) -> [a] -> Doc

type RenderFunction = JValue -> Doc

-- RenderFunction that performs compact rendering, i.e. will put multiple
-- JValues on a single line, as long as it fits their length.
renderJValue :: RenderFunction

renderJValue (JString s)   = string s
renderJValue (JNumber n)   = double n
renderJValue (JBool True)  = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull         = text "null"
renderJValue (JArray ary)  = renderJArray series renderJValue ary
renderJValue (JObject o)   = renderJObject series renderJValue o

-- RenderFunction that puts each JValue on a different line. It behaves
-- the same way as 'renderJValue' for simple types, but it handles arrays 
-- and objects differently.
renderJValueLong :: RenderFunction
renderJValueLong (JArray ary) =  renderJArray seriesLong renderJValueLong ary
renderJValueLong (JObject obj) = renderJObject seriesLong renderJValueLong obj
renderJValueLong any = renderJValue any

-- generic function for rendering arrays
renderJArray :: SeriesFunction JValue -> RenderFunction -> [JValue] -> Doc
renderJArray s = s '[' ']'

-- generic function for rendering objects
renderJObject :: SeriesFunction JPair ->RenderFunction -> [JPair] -> Doc
renderJObject s r = s '{' '}' field
    where field (name, val)  = string name
                            <> text ": "
                            <> r val

-- convert String to Doc, enclosing the value in '"'
string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

-- enclose Doc in two characters
enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right

-- enclose Doc in two strings
encloseStr :: String -> String -> Doc -> Doc
encloseStr left right x = text left <> x <> text right

-- convert Char to Doc, escaping it if necessary
oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
              Just r -> text r
              Nothing | mustEscape c -> hexEscape c
                      | otherwise    -> char c
      where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

-- list of escape rules
simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
    where ch a b = (a, ['\\', b])

smallHex :: Int -> Doc
smallHex x  = text "\\u"
           <> text (replicate (4 - length h) '0')
           <> text h
   where h = showHex x ""

astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
        where a = (n `shiftR` 10) .&. 0x3ff
              b = n .&. 0x3ff

hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex d
            | otherwise   = astral (d - 0x10000)
    where d = ord c

-- SeriesFunction that renders values compactly, i.e. possibly
-- putting multiple values on a single line.
series :: SeriesFunction a
series open close item = enclose open close 
                         . fsep . punctuate (char ',') . map item

-- SeriesFunction that puts each JValue on a separate line
seriesLong :: SeriesFunction a
seriesLong open close item = enclose open close
                         . longcat . punctuate (char ',') . map item
