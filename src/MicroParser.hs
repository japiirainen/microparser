{-# LANGUAGE DeriveFunctor #-}

module MicroParser where

import Control.Applicative (Alternative (..))
import Control.Monad (void)
import qualified Data.Char as Char
import Data.List (intercalate)

data ParseResult a
  = ParseSuccess !a !Int String
  | ParseError [(Int, String)]
  deriving (Functor, Show)

newtype Parser a = Parser {unParser :: Int -> String -> ParseResult a}

instance Functor Parser where
  fmap f (Parser p) = Parser (\i ts -> fmap f (p i ts))

instance Applicative Parser where
  pure x = Parser (ParseSuccess x)
  Parser f <*> Parser g =
    Parser
      ( \i ts ->
          case f i ts of
            ParseError es -> ParseError es
            ParseSuccess x i' ts' -> case g i' ts' of
              ParseError es' -> ParseError es'
              ParseSuccess x' i'' ts'' -> ParseSuccess (x x') i'' ts''
      )

instance Alternative Parser where
  empty = Parser (\_ _ -> ParseError [])
  Parser f <|> Parser g =
    Parser
      ( \i ts ->
          case f i ts of
            success@ParseSuccess {} -> success
            ParseError errs0 -> case g i ts of
              success@ParseSuccess {} -> success
              ParseError errs1 -> ParseError (errs0 <> errs1)
      )

satisfyMaybe :: String -> (Char -> Maybe a) -> Parser a
satisfyMaybe descr p =
  Parser
    ( \i ts -> case ts of
        (t : ts') | Just x <- p t -> ParseSuccess x (i + 1) ts'
        _ -> ParseError [(i, descr)]
    )

satisfy :: String -> (Char -> Bool) -> Parser Char
satisfy descr p = satisfyMaybe descr (\t -> if p t then Just t else Nothing)

anyChar :: Parser Char
anyChar = satisfy "any character" (const True)

char :: Char -> Parser ()
char c = void $ satisfy (show c) (== c)

string :: String -> Parser ()
string [] = pure ()
string (x : xs) = char x *> string xs

many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = (:) <$> p <*> many (sep *> p)

alpha :: Parser Char
alpha = satisfy "alpha" Char.isAlpha

newline :: Parser ()
newline = char '\n' <|> (char '\r' *> char '\n')

spaces :: Parser ()
spaces = void $ many (satisfy "whitespace" Char.isSpace)

runParser :: Parser a -> String -> Either String a
runParser (Parser p) ts = case p 0 ts of
  ParseSuccess x _ _ -> Right x
  ParseError es ->
    Left $
      "Expecting "
        <> intercalate
          " OR "
          [ e <> " at position " <> show i
            | (i, e) <- es
          ]

-- | Post 1 example
text :: String
text = "key: value"

newtype KV = KV (String, String) deriving (Show)

pKv :: Parser KV
pKv = KV <$> parseKv
  where
    parseKv = (,) <$> many alpha <* char ':' <* spaces <*> many alpha
