module Main where
import LispError
import LispVal
import Parser

import System.Environment
import Text.ParserCombinators.Parsec

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
	Left err -> LispString $ "No match: " ++ show err
	Right val -> val

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
