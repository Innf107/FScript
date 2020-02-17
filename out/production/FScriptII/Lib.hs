module Lib where

import Text.Parsec (ParseError)
import Text.Parsec.String (Parser)
import Text.Parsec.String.Parsec
import Text.Parsec.String.Char
import Text.Parsec.String.Combinator
import Control.Applicative ((<|>), many)
import Data.List as L

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x)  = (Left (f x))
mapLeft _ (Right x) = (Right x)


parseMaybe :: Parser a -> Parser (Maybe a)
parseMaybe p = Just <$> try p <|> return Nothing

unique :: Eq a => [a] -> [a] -> [a]
unique [] x      = x
unique x []      = x
unique xs (y:ys) = unique (filter (/=y) xs) ys

notP :: (Show a) => Parser a -> Parser a -> Parser a
notP n p = (>>=) (((,)False <$> try n) <|> ((,)True <$> p)) $ \s -> case s of
    (True, s)  -> return s
    (False, s) -> fail $ "symbol '" ++ show s ++ "' is reserved!"

anyP :: [Parser a] -> Parser a
anyP []     = fail "none of the parsers matched!"
anyP (p:ps) = try p <|> anyP ps

noPof :: (Show a) => [Parser a] -> Parser a -> Parser a
noPof [] p = p
noPof ns p = notP (anyP ns) p  

setAL :: (Eq k) => k -> v -> [(k, v)] -> [(k, v)]
setAL k v []     = [(k, v)]
setAL k v ((k', v'):xs)
    | k == k'    = ((k, v):xs)
    | otherwise  = ((k', v'):(setAL k v xs))



(|>) :: a -> (a -> b) -> b
x |> f = f x
