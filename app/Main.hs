module Main where

import Data.Char
import Control.Applicative

data JsonValue
    = JsonNull
    | JsonBool Bool
    | JsonNumber Int
    | JsonString String
    | JsonArray [JsonValue]
    | JsonObject [(String, JsonValue)]
    deriving (Show, Eq)

newtype Parser a = Parser {
    runParser :: String -> Maybe (String, a)
    }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> do
        (input', a) <- p input
        Just (input', f a)

instance Applicative Parser where
    pure x = Parser $ \input -> Just (input, x)
    (Parser p1) <*> (Parser p2) = Parser $ \input -> do
        (input', f) <- p1 input
        (input'', a) <- p2 input'
        Just (input'', f a)

instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    (Parser p1) <|> (Parser p2) = Parser $ \input ->
        p1 input <|> p2 input
    
charP :: Char -> Parser Char
charP x = Parser f
    where
        f (y:ys)
            | y == x = Just (ys, x)
            | otherwise = Nothing
        f [] = Nothing

stringP :: String -> Parser String
stringP =  traverse charP

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input ->
    let (token, rest) = span f input
    in Just (rest, token)

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ \input -> do
    (input', xs) <- p input
    if null xs
        then Nothing
        else Just (input', xs)

ws :: Parser String
ws = spanP isSpace

stringLiteral :: Parser String
stringLiteral = charP '"' *> spanP (/= '"') <* charP '"'

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

jsonNull :: Parser JsonValue
jsonNull = (\_ -> JsonNull) <$> stringP "null"

jsonBool :: Parser JsonValue
jsonBool = ((\_ -> JsonBool True) <$> stringP "true") <|> ((\_ -> JsonBool False) <$> stringP "false")

jsonNumber :: Parser JsonValue
jsonNumber = JsonNumber . read <$> notNull (spanP isDigit)

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (charP '[' *> ws *> sepBy (ws *> charP ',' <* ws) jsonValue <* ws <* charP ']')

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> (charP '{' *> ws *> sepBy (ws *> charP ',' <* ws) ((\key _ value -> (key, value)) <$> stringLiteral <*> (ws *> charP ':' <* ws) <*> jsonValue) <* ws <* charP '}')

jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject

parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile fileName parser = do
    input <- readFile fileName
    return (snd <$> runParser parser input)

main :: IO ()
main = putStrLn "Hello, Haskell!"
