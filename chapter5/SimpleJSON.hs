module SimpleJSON where

type JPair = (String, JValue)

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [JPair]
            | JArray [JValue]
            deriving (Eq, Ord, Show)

getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _           = Nothing

getInt :: JValue -> Maybe Int
getInt (JNumber n) = Just (truncate n)
getInt _           = Nothing

getBool :: JValue -> Maybe Bool
getBool (JBool b) = Just b
getBool _         = Nothing

getObject :: JValue -> Maybe [(String, JValue)]
getObject (JObject o) = Just o
getObject _           = Nothing

getArray :: JValue -> Maybe [JValue]
getArray (JArray s) = Just s
getArray _          = Nothing

isNull :: JValue -> Bool
isNull v = v == JNull
