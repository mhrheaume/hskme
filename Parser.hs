module Parser (
	parseExpr
) where

import LispVal

import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

parseAtom :: Parser LispVal
parseAtom = do
	first <- letter <|> symbol
	rest <- many (letter <|> digit <|> symbol)
	return $ LispAtom (first:rest)

parseChar :: Parser LispVal
parseChar = do
	char <-  parseSpace <|> parseNewline <|> parseAnyChar
	return $ LispChar char
	where
		parseSpace = try(string "space") >>= (\_ -> return '_')
		parseNewline = try(string "newline") >>= (\_ -> return '\n')
		parseAnyChar = do
			c <- anyChar
			notFollowedBy $ noneOf " \n()\";"
			return c

parseBoolChar :: Parser LispVal
parseBoolChar = do
	char '#'
	x <- oneOf "tf\\" <?> "bool or char"
	case x of
		't' -> return $ LispBool True
		'f' -> return $ LispBool False
		'\\' -> parseChar

parseList :: Parser LispVal
parseList = liftM LispList $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
	head <- endBy parseExpr spaces
	tail <- char '.' >> spaces >> parseExpr
	return $ LispDottedList head tail

parseNumber :: Parser LispVal
parseNumber = liftM (LispNumber . read) $ many1 digit

parseString :: Parser LispVal
parseString = do
	char '"'
	x <- many (noneOf "\"")
	char '"'
	return $ LispString x

parseQuoted :: Parser LispVal
parseQuoted = do
	char '\''
	x <- parseExpr
	return $ LispList [LispAtom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseBoolChar <|>
	parseAtom <|>
	parseString <|>
	parseNumber <|>
	parseQuoted <|> do
	char '('
	x <- try parseList <|> parseDottedList
	char ')'
	return x
