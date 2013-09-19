module IOHelpers (
	readExpr,
	readExprList
) where

import IError
import LispVal
import Parser

import Control.Monad.Error
import Text.ParserCombinators.Parsec

readOrThrow :: Parser a -> String -> ThrowsLispError a
readOrThrow parser input = case parse parser "lisp" input of
	Left err -> throwError $ Parser err
	Right val -> return val

readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)

