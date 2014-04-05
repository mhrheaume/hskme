module StringPrimitives (
	strPrimitives
) where

import IError
import LispVal
import Primitives

import Control.Monad.Error

strPrimitives :: [(String, [LispVal] -> ThrowsLispError LispVal)]
strPrimitives =
	[("string?", isStr),
	 ("string-length", strLength),
	 ("string=?", strBoolBinop (==)),
	 ("string<?", strBoolBinop (<)),
	 ("string>?", strBoolBinop (>)),
	 ("string<=?", strBoolBinop (<=)),
	 ("string>=?", strBoolBinop (>=))]

strBoolBinop = boolBinop unpackStr

unpackStr :: LispVal -> ThrowsLispError String
unpackStr (LispString s) = return s
unpackStr (LispNumber s) = return $ show s
unpackStr (LispBool s) = return $ show s
unpackStr other = throwError $ TypeMismatch "string" other

isStr :: [LispVal] -> ThrowsLispError LispVal
isStr [LispString s] = return $ LispBool True
isStr [_] = return $ LispBool False
isStr args = throwError $ NumArgs 1 args

strLength :: [LispVal] -> ThrowsLispError LispVal
strLength [LispString s] = return . LispNumber . fromIntegral $ length s
strLength [arg] = throwError $ TypeMismatch "string" arg
strLength args = throwError $ NumArgs 1 args
