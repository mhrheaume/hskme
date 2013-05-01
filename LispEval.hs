module LispEval (
	eval
) where

import LispVal
import LispError

import Control.Monad.Error

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
	[("+", numericBinop (+)),
	 ("-", numericBinop (-)),
	 ("*", numericBinop (*)),
	 ("/", numericBinop div),
	 ("mod", numericBinop mod),
	 ("quotient", numericBinop quot),
	 ("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . LispNumber . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (LispNumber n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe
	(throwError $ NotFunction "Unrecognized primitive function args" func)
	($ args)
	(lookup func primitives)

eval :: LispVal -> ThrowsError LispVal
eval val@(LispString _) = return val
eval val@(LispNumber _) = return val
eval val@(LispBool _) = return val
eval (LispList [LispAtom "quote", val]) = return val
eval (LispList (LispAtom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm
