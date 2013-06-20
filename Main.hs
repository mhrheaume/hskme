module Main where

import Environment
import Eval
import IError
import LispVal
import Parser
import Primitives

import Control.Monad.Error
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: (Environment LispVal) -> String -> IO String
evalString env expr = runIOThrows $ liftM show $
	(liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: (Environment LispVal) -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
	result <- prompt
	if pred result
		then return ()
		else action result >> until_ pred prompt action

primBindings :: IO (Environment LispVal)
primBindings = nullEnv >>= (flip bindVars $ map makePrimFunc primitives) where
	makePrimFunc (var, func) = (var, PrimitiveFunc func)

runOne :: String -> IO ()
runOne expr = primBindings >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl = primBindings >>= inputEvalLoop where
	inputEvalLoop = until_ (== "quit") (readPrompt "hskme>>> ") . evalAndPrint

readExpr :: String -> ThrowsLispError LispVal
readExpr input = case parse parseExpr "lisp" input of
	Left err -> throwError $ Parser err
	Right val -> return val

main :: IO ()
main = do
	args <- getArgs
	case length args of
		0 -> runRepl
		1 -> runOne $ args !! 0
		otherwise -> putStrLn "Program takes only 0 or 1 argument"
