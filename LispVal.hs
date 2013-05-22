module LispVal (
	LispVal(
		LispAtom,
		LispList,
		LispDottedList,
		LispNumber,
		LispString,
		LispBool
	),
	showVal,
	unwordsList
) where

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
				, closure :: Env
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

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal
