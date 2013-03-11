module Parser (parseExpr) where
import LispVal

import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseAtom :: Parser LispVal
parseAtom = do
	first <- letter <|> symbol
	rest <- many (letter <|> digit <|> symbol)
	let atom = first:rest
	return $ case atom of
		"#t" -> LispBool True
		"#f" -> LispBool False
		_ -> LispAtom atom

parseList :: Parser LispVal
parseList = liftM LispList $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
	head <- endBy parseExpr spaces
	tail <- char '.' >> spaces >> parseExpr
	return $ LispDottedList head tail

parseNumber :: Parser LispVal
-- parseNumber = liftM (LispNumber . read) $ many1 digit
parseNumber = do
	num <- many1 digit
	return $ (LispNumber . read) num

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
parseExpr = parseAtom <|> parseString <|> parseNumber <|> parseQuoted <|> do
	char '('
	x <- try parseList <|> parseDottedList
	char ')'
	return x
