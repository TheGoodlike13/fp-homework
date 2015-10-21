module JsonParser where

import Data.Char
import Data.List
import Data.Maybe
import Text.Read

class Json a where
    toJson :: a -> JsonValue
    fromJson :: JsonValue -> Maybe a

    encodeJson :: a -> String
    encodeJson jsonObject = show (toJson jsonObject)
    parseJson :: String -> Maybe a
    parseJson jsonString = parse jsonString >>= fromJson

data JsonPair = JsonPair (String, JsonValue) deriving (Eq)
data JsonValue = JsonArray [JsonValue]
                    | JsonObject [JsonPair]
                    | JsonString String
                    | JsonInt Int
                    | JsonInteger Integer
                    | JsonDouble Double
                    deriving (Eq)

instance Show JsonPair where
    show (JsonPair (string, jsonValue)) = "\"" ++ string ++ "\": " ++ show jsonValue

instance Show JsonValue where
    show (JsonString str) = "\"" ++ str ++ "\""
    show (JsonInt int) = show int
    show (JsonInteger int) = show int
    show (JsonDouble double) = show double
    show (JsonArray list) = "[" ++ intercalate ", " [show e | e <- list] ++ "]"
    show (JsonObject list) = "{" ++ intercalate ", " [show e | e <- list] ++ "}"

parse :: String -> Maybe JsonValue
parse any
    | isFinished parseResult = result
    | otherwise = Nothing
    where
        parseResult = parseNextValue any
        result = fmap fst parseResult

parseNextValue :: String -> Maybe (JsonValue, String)
parseNextValue "" = Nothing
parseNextValue (char : rest)
    | isSpace char = parseNextValue rest
    | char == '[' = parseNextArray rest
    | char == '{' = parseNextObject rest
    | char == '"' = parseNextString rest
    | isDigit char || char == '-' = parseNextNumber (char : rest)
    | otherwise = Nothing

parseNextArray :: String -> Maybe (JsonValue, String)
parseNextArray any = parseNextArrayWithResults any Nothing []

parseNextArrayWithResults :: String -> Maybe JsonValue -> [JsonValue] -> Maybe (JsonValue, String)
parseNextArrayWithResults "" _ _ = Nothing
parseNextArrayWithResults (',' : rest) lastMem fullMem
    = lastMem >>= (\mem -> parseNextArrayWithResults rest Nothing (mem : fullMem))
parseNextArrayWithResults (']' : rest) Nothing [] = Just (JsonArray [], rest)
parseNextArrayWithResults (']' : rest) lastMem fullMem
    = lastMem >>= (\lastValue -> Just (JsonArray (reverse (lastValue : fullMem)), rest))
parseNextArrayWithResults (char : rest) lastMem fullMem
    | isSpace char = parseNextArrayWithResults rest lastMem fullMem
parseNextArrayWithResults json (Just _) _ = Nothing
parseNextArrayWithResults json _ fullMem
    = parseNextValue json >>= (\value -> parseNextArrayWithResults (snd value) (Just (fst value)) fullMem)

parseNextObject :: String -> Maybe (JsonValue, String)
parseNextObject any = parseNextObjectWithResults any Nothing []

parseNextString :: String -> Maybe (JsonValue, String)
parseNextString any = parseNextStringWithResult any []

parseNextStringWithResult :: String -> String -> Maybe (JsonValue, String)
parseNextStringWithResult "" _ = Nothing
parseNextStringWithResult ('"' : rest) mem = Just (JsonString (reverse mem), rest)
parseNextStringWithResult [char] _ = Nothing
parseNextStringWithResult ('\\' : escaped : rest) mem
    | escaped == '"' = parseNextStringWithResult rest ('"' : mem)
    | escaped == '\\' = parseNextStringWithResult rest ('\\' : mem)
    | escaped == '/' = parseNextStringWithResult rest ('/' : mem)
    | escaped == 'b' = parseNextStringWithResult rest ('\b' : mem)
    | escaped == 'f' = parseNextStringWithResult rest ('\f' : mem)
    | escaped == 'n' = parseNextStringWithResult rest ('\n' : mem)
    | escaped == 'r' = parseNextStringWithResult rest ('\r' : mem)
    | escaped == 't' = parseNextStringWithResult rest ('\t' : mem)
parseNextStringWithResult ('\\' : 'u' : hex1 : hex2 : hex3 : hex4 : rest) mem
    | all isHexDigit hexStr = parseNextStringWithResult rest ((charFromUnicode hexStr) : mem)
    where hexStr = [hex1, hex2, hex3, hex4]
parseNextStringWithResult ('\\' : rest) _ = Nothing
parseNextStringWithResult (char : rest) mem
    | isControl char = Nothing
    | otherwise = parseNextStringWithResult rest (char : mem)

parseNextNumber :: String -> Maybe (JsonValue, String)
parseNextNumber any = parseNextNumberWithResult any [] toInt

parseNextPair :: String -> Maybe (JsonPair, String)
parseNextPair (char : rest)
    | isSpace char = Nothing
    | char == '"' && hasKey = parseValueInPair keyRemains (extract key)
    | otherwise = Nothing
    where
        parsedKey = parseNextString rest
        hasKey = isJust parsedKey
        justKey = fromJust parsedKey
        key = fst justKey
        keyRemains = snd justKey

parseNextObjectWithResults :: String -> Maybe JsonPair -> [JsonPair] -> Maybe (JsonValue, String)
parseNextObjectWithResults "" _ _ = Nothing
parseNextObjectWithResults (',' : rest) lastMem fullMem
    | isNothing lastMem = Nothing
    | otherwise = parseNextObjectWithResults rest Nothing (value : fullMem)
    where value = fromJust lastMem
parseNextObjectWithResults ('}' : rest) lastMem fullMem
    | isNothing lastMem && null fullMem = Just (JsonObject [], rest)
    | isNothing lastMem = Nothing
    | otherwise = Just (JsonObject (reverse mem), rest)
    where
        value = fromJust lastMem
        mem = value : fullMem
parseNextObjectWithResults (char : rest) lastMem fullMem
    | isSpace char = parseNextObjectWithResults rest lastMem fullMem
    | isJust lastMem = Nothing
    | isNothing nextPair = Nothing
    | otherwise = parseNextObjectWithResults remains (Just pair) fullMem
    where
        nextPair = parseNextPair (char : rest)
        justPair = fromJust nextPair
        pair = fst justPair
        remains = snd justPair

parseNextNumberWithResult :: String -> String -> (String -> Maybe JsonValue) -> Maybe (JsonValue, String)
parseNextNumberWithResult "" "" _ = Nothing
parseNextNumberWithResult "" mem converter = result
    where
        memStr = reverse mem
        value = fmap reduceInteger (converter memStr)
        result = fmap (\m -> (m, "")) value
parseNextNumberWithResult (char : rest) mem converter
    | isDigit char || char == '-' || char == '+' = parseNextNumberWithResult rest (char : mem) converter
    | char == 'e' || char == '.' = parseNextNumberWithResult rest (char : mem) toDouble
    | otherwise = result
    where
        memStr = reverse mem
        value = fmap reduceInteger (converter memStr)
        result = fmap (\m -> (m, (char : rest))) value

reduceInteger :: JsonValue -> JsonValue
reduceInteger (JsonInteger int)
    | int <= maxValue && int >= minValue = JsonInt convertedValue
    where
        minValue = toInteger (minBound :: Int)
        maxValue = toInteger (maxBound :: Int)
        convertedValue = fromInteger int :: Int
reduceInteger anyOther = anyOther

parseValueInPair :: String -> String -> Maybe (JsonPair, String)
parseValueInPair "" _ = Nothing
parseValueInPair (':' : rest) key
    | hasValue = Just (JsonPair (key, value), remains)
    | otherwise = Nothing
    where
        parsedValue = parseNextValue rest
        hasValue = isJust parsedValue
        justValue = fromJust parsedValue
        value = fst justValue
        remains = snd justValue

charFromUnicode :: String -> Char
charFromUnicode (hex1 : hex2 : hex3 : hex4 : []) = (fst . head . readLitChar) ['\\', 'x', hex1, hex2, hex3, hex4]
charFromUnicode _ = error "Illegal format!"

toInt :: String -> Maybe JsonValue
toInt str = fmap JsonInteger (readMaybe str :: Maybe Integer)

toDouble :: String -> Maybe JsonValue
toDouble str = fmap JsonDouble (readMaybe str :: Maybe Double)

isFinished :: Maybe (JsonValue, String) -> Bool
isFinished Nothing = True
isFinished (Just (_, remains)) = null remains || all isSpace remains

extract :: JsonValue -> String
extract (JsonString string) = string
extract _ = error "Extracting intended for strings only"
