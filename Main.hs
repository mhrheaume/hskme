module Main where

import LispEval
import LispError
import LispVal
import Parser

import Control.Monad.Error
import System.Environment
import Text.ParserCombinators.Parsec

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
	Left err -> throwError $ Parser err
	Right val -> return val

main :: IO ()
main = do
	args <- getArgs
	evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
	putStrLn $ extractValue $ trapError evaled
