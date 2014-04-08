module StringPrimitives (
	charPrimitives,
	strPrimitives
) where

import IError
import LispVal
import Primitives

import Control.Monad.Error
import Data.Char

charPrimitives :: [(String, [LispVal] -> ThrowsLispError LispVal)]
charPrimitives =
	[("char?", isChar),
	 ("char=?", charBoolBinop (==)),
	 ("char<?", charBoolBinop (<)),
	 ("char>?", charBoolBinop (>)),
	 ("char<=?", charBoolBinop (<=)),
	 ("char>=?", charBoolBinop (>=)),
	 ("char->integer", charToInt),
	 ("integer->char", intToChar)]

isChar :: [LispVal] -> ThrowsLispError LispVal
isChar [LispChar c] = return $ LispBool True
isChar [_] = return $ LispBool False
isChar args = throwError $ NumArgs 1 args

charBoolBinop = boolBinop unpackChar

unpackChar :: LispVal -> ThrowsLispError Char
unpackChar (LispChar c) = return c
unpackChar other = throwError $ TypeMismatch "char" other

charToInt :: [LispVal] -> ThrowsLispError LispVal
charToInt [LispChar c] = return . LispNumber . fromIntegral $ ord c
charToInt [arg]  = throwError $ TypeMismatch "char" arg
charToInt args = throwError $ NumArgs 1 args

intToChar :: [LispVal] -> ThrowsLispError LispVal
intToChar [LispNumber n]
	| n < 256 = return . LispChar . chr $ fromIntegral n
	| otherwise = throwError $ InvalidArgument 1 "integer out of range"
intToChar [arg] = throwError $ TypeMismatch "number" arg
intToChar args = throwError $ NumArgs 1 args

strPrimitives :: [(String, [LispVal] -> ThrowsLispError LispVal)]
strPrimitives =
	[("string?", isStr),
	 ("make-string", makeStr),
	 ("string-length", strLength),
	 ("string-ref", strRef),
	 ("string=?", strBoolBinop (==)),
	 ("string<?", strBoolBinop (<)),
	 ("string>?", strBoolBinop (>)),
	 ("string<=?", strBoolBinop (<=)),
	 ("string>=?", strBoolBinop (>=))]

isStr :: [LispVal] -> ThrowsLispError LispVal
isStr [LispString s _] = return $ LispBool True
isStr [_] = return $ LispBool False
isStr args = throwError $ NumArgs 1 args

makeStr :: [LispVal] -> ThrowsLispError LispVal
makeStr [ln@(LispNumber n)] = makeStr [ln, LispChar '\0']
makeStr [LispNumber n, LispChar c] = return $ LispString (genString n c) True where
	genString 0 c = []
	genString n c = [c] ++ genString (n - 1) c
makeStr [LispNumber n, arg] = throwError $ TypeMismatch "char" arg
makeStr [arg, _] = throwError $ TypeMismatch "integer" arg
makeStr [arg] = throwError $ TypeMismatch "integer" arg
makeStr args = throwError $ NumArgs 1 args

strLength :: [LispVal] -> ThrowsLispError LispVal
strLength [LispString s _] = return . LispNumber . fromIntegral $ length s
strLength [arg] = throwError $ TypeMismatch "string" arg
strLength args = throwError $ NumArgs 1 args

strRef :: [LispVal] -> ThrowsLispError LispVal
strRef [LispString s _, LispNumber n]
	| 0 <= n && n < fromIntegral (length s) = return . LispChar $ s !! fromIntegral n
	| otherwise = throwError $ InvalidArgument 2 "index out of range"
strRef [LispString s _, arg] = throwError $ TypeMismatch "number" arg
strRef [arg, _] = throwError $ TypeMismatch "string" arg
strRef args = throwError $ NumArgs 2 args

strBoolBinop = boolBinop unpackStr

unpackStr :: LispVal -> ThrowsLispError String
unpackStr (LispString s _) = return s
unpackStr (LispNumber s) = return $ show s
unpackStr (LispBool s) = return $ show s
unpackStr other = throwError $ TypeMismatch "string" other
