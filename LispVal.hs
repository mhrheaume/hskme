module LispVal (
	LispVal(
		LispAtom,
		LispList,
		LispDottedList,
		LispNumber,
		LispString,
		LispBool,
		PrimitiveFunc,
		Func
	),
	makeFunc,
	makeNormalFunc,
	makeVargsFunc
) where

import IError
import Environment
import Util

data LispVal = LispAtom String
			 | LispList [LispVal]
			 | LispDottedList [LispVal] LispVal
			 | LispNumber Integer
			 | LispString String
			 | LispBool Bool
			 | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
			 | Func
				{ params :: [String]
				, vararg :: (Maybe String)
				, body :: [LispVal]
				, closure :: Environment LispVal
				}

showVal :: LispVal -> String
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

instance Show LispVal where show = showVal

makeFunc :: (Maybe String)
	-> (Environment LispVal)
	-> [LispVal]
	-> [LispVal]
	-> IOThrowsError LispVal
makeFunc vargs env params body = return $ Func (map showVal params) vargs body env

makeNormalFunc :: (Environment LispVal)
	-> [LispVal]
	-> [LispVal]
	-> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing

makeVargsFunc :: LispVal
	-> (Environment LispVal)
	-> [LispVal]
	-> [LispVal]
	-> IOThrowsError LispVal
makeVargsFunc = makeFunc . Just . showVal

