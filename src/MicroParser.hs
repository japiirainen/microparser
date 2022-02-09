{-# LANGUAGE DeriveFunctor #-}

module MicroParser where

import Control.Applicative (Alternative (..), optional)
import Control.Monad (void)
import qualified Data.Char as Char
import Data.Functor (($>))
import Data.List (intercalate)
import Data.Maybe (fromMaybe)

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

alpha :: Parser Char
alpha = satisfy "alpha" Char.isAlpha

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy parser separator = sepBy1 parser separator <|> pure []

digit :: Parser Char
digit = satisfy "digit" Char.isDigit

decimal :: (Integral a, Read a) => Parser a
decimal = read <$> many1 digit

signedDecimal :: Parser Int
signedDecimal = fromMaybe id <$> optional (char '-' $> negate) <*> decimal

spaces :: Parser ()
spaces = void $ many (satisfy "whitespace" Char.isSpace)

newline :: Parser ()
newline = char '\n' <|> (char '\r' *> char '\n')

horizontalSpaces :: Parser ()
horizontalSpaces = void . many $
  satisfy "horizontal whitespace" $ \c ->
    Char.isSpace c && c /= '\n' && c /= '\r'

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

-- Part 2 examples
pKvs :: Parser [KV]
pKvs = pKv `sepBy` char ','

pKvsNewLine :: Parser [KV]
pKvsNewLine = pKv `sepBy` newline

pKvsHorizontal :: Parser [KV]
pKvsHorizontal = pKv `sepBy` s
  where
    s = horizontalSpaces *> char ',' <* horizontalSpaces
