module StringPrimitives (
	strPrimitives
) where

import IError
import LispVal
import Primitives

import Control.Monad.Error

strPrimitives :: [(String, [LispVal] -> ThrowsLispError LispVal)]
strPrimitives =
	[("string=?", strBoolBinop (==)),
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
