module Main where

import Environment
import Eval
import IError
import IOPrimitives
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
primBindings = nullEnv >>= (flip bindVars $ allFuncs) where
	allFuncs = primFuncs ++ ioFuncs
	primFuncs = map (makeFunc PrimitiveFunc) primitives
	ioFuncs = map (makeFunc IOFunc) ioPrimitives
	makeFunc constr (var, func) = (var, constr func)

map makePrimFunc primitives) where
	makePrimFunc (var, func) = (var, PrimitiveFunc func)

runOne :: [String] -> IO ()
runOne args = do
	env <- primBindings >>= flip bindVars argList
	(runIOThrows $ liftM show $ runFile) >>= hPutStrLn stderr
	where
		argList = [("args", LispList $ map String $ drop 1 args)]
		runFile = eval env (LispList [LispAtom "load", String (args !! 0)])

runRepl :: IO ()
runRepl = primBindings >>= inputEvalLoop where
	inputEvalLoop = until_ (== "quit") (readPrompt "hskme>>> ") . evalAndPrint

readOrThrow :: Parser a -> String -> ThrowsLispError a
readOrThrow parser input = case parse parser "lisp" input of
	Left err -> throwError $ Parser err
	Right val -> return val

readExpr :: String -> ThrowsLispError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsLispError LispVal
readExprList = readOrThrow (endBy parseExpr spaces)

readExpr :: String -> ThrowsLispError LispVal
readExpr input = case parse parseExpr "lisp" input of
	Left err -> throwError $ Parser err
	Right val -> return val

main :: IO ()
main = do
	args <- getArgs
	if null args then runRepl else runOne $ args
