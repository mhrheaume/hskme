module LispVal (
	LispVal(
		LispAtom,
		LispList,
		LispDottedList,
		LispNumber,
		LispChar,
		LispString,
		LispBool,
		PrimitiveFunc,
		Func,
		IOFunc,
		Port
	),
	ThrowsLispError,
	IOThrowsLispError,
	makeFunc,
	makeNormalFunc,
	makeVargsFunc
) where

import IError
import Environment
import Util

import Control.Monad.Error
import System.IO

data LispVal = LispAtom String
			 | LispList [LispVal]
			 | LispDottedList [LispVal] LispVal
			 | LispNumber Integer
			 | LispChar Char
			 | LispString String
			 | LispBool Bool
			 | PrimitiveFunc ([LispVal] -> ThrowsLispError LispVal)
			 | Func
				{ params :: [String]
				, vararg :: (Maybe String)
				, body :: [LispVal]
				, closure :: Environment LispVal
				}
			 | IOFunc ([LispVal] -> IOThrowsLispError LispVal)
			 | Port Handle

type ThrowsLispError a = ThrowsError LispVal a
type IOThrowsLispError a = IOThrowsError LispVal a

showVal :: LispVal -> String
showVal (LispChar char) = "#\\" ++ [char]
showVal (LispString contents) = "\"" ++ contents ++ "\""
showVal (LispAtom name) = name
showVal (LispNumber contents) = show contents
showVal (LispBool True) = "#t"
showVal (LispBool False) = "#f"
showVal (LispList contents) = "(" ++ unwordsList contents ++ ")"
showVal (LispDottedList head tail) = "(" ++ h ++ " . " ++ t where
	h = unwordsList head
	t = showVal tail
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
	"(lambda (" ++ unwords (map show args) ++ vaStr ++ ") ...)" where
		vaStr = case varargs of
			Nothing -> ""
			Just arg -> " . " ++ arg
showVal (IOFunc _) = "<IO primitive>"
showVal (Port _) = "<IO port>"

instance Show LispVal where show = showVal

makeFunc :: (Maybe String)
	-> (Environment LispVal)
	-> [LispVal]
	-> [LispVal]
	-> IOThrowsLispError LispVal
makeFunc vargs env params body =
	return $ Func (map showVal params) vargs body env

makeNormalFunc :: (Environment LispVal)
	-> [LispVal]
	-> [LispVal]
	-> IOThrowsLispError LispVal
makeNormalFunc = makeFunc Nothing

makeVargsFunc :: LispVal
	-> (Environment LispVal)
	-> [LispVal]
	-> [LispVal]
	-> IOThrowsLispError LispVal
makeVargsFunc = makeFunc . Just . showVal
