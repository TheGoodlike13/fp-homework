module JSON where

import Data.Char
import qualified Data.Char as Char

import Data.Maybe
import qualified Data.Maybe as Maybe

import Data.List

type JsonPair = (String, JsonValue)
data JsonValue = JsonArray [JsonValue] | JsonObject [JsonPair] | JsonString String | JsonInt Int

parse :: String -> Maybe JsonValue
parse any
    | isFinished parseResult = result
    | otherwise = Nothing
    where
        parseResult = parseNextValue any
        result = fmap fst parseResult

-- toString

toStr :: JsonValue -> String
toStr (JsonString str) = str
toStr (JsonInt int) = show int
toStr (JsonArray []) = "[]"
toStr (JsonArray list) = "[" ++ formattedStr [toStr e | e <- list] ++ "]"
toStr (JsonObject []) = "{}"
toStr (JsonObject list) = "{" ++ formattedStr [toStrPair e | e <- list] ++ "}"

toStrPair :: JsonPair -> String
toStrPair (string, jsonValue) = string ++ ":" ++ toStr jsonValue

formattedStr :: [String] -> String
formattedStr = intercalate ", "

-- parsers

parseNextValue :: String -> Maybe (JsonValue, String)
parseNextValue "" = Nothing
parseNextValue (char : rest)
    | isSpace char = parseNextValue rest
    | char == '[' = parseNextArray rest
    | char == '{' = parseNextObject rest
    | char == '"' = parseNextString rest
    | isDigit char = parseNextInt (char : rest)
    | otherwise = Nothing

parseNextArray :: String -> Maybe (JsonValue, String)
parseNextArray any = parseNextArrayWithResults any Nothing []

parseNextObject :: String -> Maybe (JsonValue, String)
parseNextObject any = parseNextObjectWithResults any Nothing []

parseNextString :: String -> Maybe (JsonValue, String)
parseNextString any = parseNextStringWithResult any []

parseNextInt :: String -> Maybe (JsonValue, String)
parseNextInt any = parseNextIntWithResult any []

parseNextPair :: String -> Maybe (JsonPair, String)
parseNextPair (char : rest)
    | isSpace char = Nothing
    | char == '"' && hasKey = parseValueInPair keyRemains (toStr key)
    | otherwise = Nothing
    where
        parsedKey = parseNextString rest
        hasKey = isJust parsedKey
        justKey = fromJust parsedKey
        key = fst justKey
        keyRemains = snd justKey

-- helpers

parseNextStringWithResult :: String -> String -> Maybe (JsonValue, String)
parseNextStringWithResult "" _ = Nothing
parseNextStringWithResult ['\"'] mem = Just (JsonString (reverse mem), "")
parseNextStringWithResult [char] _ = Nothing
parseNextStringWithResult (char : rest) _ | isControl char = Nothing
parseNextStringWithResult ('\\' : '"' : rest) mem = parseNextStringWithResult rest ('"' : mem)
parseNextStringWithResult ('\\' : '\\' : rest) mem = parseNextStringWithResult rest ('\\' : mem)
parseNextStringWithResult ('\\' : '/' : rest) mem = parseNextStringWithResult rest ('/' : mem)
parseNextStringWithResult ('\\' : 'b' : rest) mem = parseNextStringWithResult rest ('\b' : mem)
parseNextStringWithResult ('\\' : 'f' : rest) mem = parseNextStringWithResult rest ('\f' : mem)
parseNextStringWithResult ('\\' : 'n' : rest) mem = parseNextStringWithResult rest ('\n' : mem)
parseNextStringWithResult ('\\' : 'r' : rest) mem = parseNextStringWithResult rest ('\r' : mem)
parseNextStringWithResult ('\\' : 't' : rest) mem = parseNextStringWithResult rest ('\t' : mem)
parseNextStringWithResult ('\\' : 'u' : hex1 : hex2 : hex3 : hex4 : rest) mem
    | all isHexDigit hexStr = parseNextStringWithResult rest ((charFromUnicode hexStr) : mem)
    where hexStr = [hex1, hex2, hex3, hex4]
parseNextStringWithResult ('\\' : rest) _ = Nothing
parseNextStringWithResult (char : '"' : rest) mem = Just (JsonString (reverse (char : mem)), rest)
parseNextStringWithResult (char : rest) mem = parseNextStringWithResult rest (char : mem)

parseNextIntWithResult :: String -> String -> Maybe (JsonValue, String)
parseNextIntWithResult "" "" = Nothing
parseNextIntWithResult "" mem = Just ((toInt . reverse) mem, "")
parseNextIntWithResult (char : rest) mem
    | isDigit char = parseNextIntWithResult rest (char : mem)
    | otherwise = Just ((toInt . reverse) mem, (char : rest))

parseNextArrayWithResults :: String -> Maybe JsonValue -> [JsonValue] -> Maybe (JsonValue, String)
parseNextArrayWithResults "" _ _ = Nothing
parseNextArrayWithResults (',' : rest) lastMem fullMem
    | isNothing lastMem = Nothing
    | otherwise = parseNextArrayWithResults rest Nothing (value : fullMem)
    where value = fromJust lastMem
parseNextArrayWithResults (']' : rest) lastMem fullMem
    | isNothing lastMem && null fullMem = Just (JsonArray [], rest)
    | isNothing lastMem = Nothing
    | otherwise = Just (JsonArray (reverse mem), rest)
    where
        value = fromJust lastMem
        mem = value : fullMem
parseNextArrayWithResults (char : rest) lastMem fullMem
    | isSpace char = parseNextArrayWithResults rest lastMem fullMem
    | isJust lastMem = Nothing
    | isNothing nextValue = Nothing
    | otherwise = parseNextArrayWithResults remains (Just value) fullMem
    where
        nextValue = parseNextValue (char : rest)
        justValue = fromJust nextValue
        value = fst justValue
        remains = snd justValue

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

parseValueInPair :: String -> String -> Maybe (JsonPair, String)
parseValueInPair "" _ = Nothing
parseValueInPair (':' : rest) key
    | hasValue = Just ((key, value), remains)
    | otherwise = Nothing
    where
        parsedValue = parseNextValue rest
        hasValue = isJust parsedValue
        justValue = fromJust parsedValue
        value = fst justValue
        remains = snd justValue

-- simple helpers

charFromUnicode :: String -> Char
charFromUnicode (hex1 : hex2 : hex3 : hex4 : []) = (fst . head . readLitChar) ['\\', 'x', hex1, hex2, hex3, hex4]
charFromUnicode _ = error "Illegal format!"

toInt :: String -> JsonValue
toInt number = JsonInt (read number :: Int)

isFinished :: Maybe (JsonValue, String) -> Bool
isFinished Nothing = True
isFinished (Just (_, remains)) = null remains || all isSpace remains

-- test

p :: String -> IO()
p string
    | isJust json = putStr (toStr jsonValue ++ "\n")
    | otherwise = putStr "Nothing\n"
    where
        json = parse string
        jsonValue = fromJust json
