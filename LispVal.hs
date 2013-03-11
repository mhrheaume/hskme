module LispVal (
	LispVal(LispAtom, LispList, LispDottedList, LispNumber, LispString, LispBool),
	showVal,
	unwordsList,
	eval
) where

data LispVal = LispAtom String
			 | LispList [LispVal]
			 | LispDottedList [LispVal] LispVal
			 | LispNumber Integer
			 | LispString String
			 | LispBool Bool

primitives :: [(String, [LispVal] -> LispVal)]
primitives =
	[("+", numericBinop (+)),
	 ("-", numericBinop (-)),
	 ("*", numericBinop (*)),
	 ("/", numericBinop div),
	 ("mod", numericBinop mod),
	 ("quotient", numericBinop quot),
	 ("remainder", numericBinop rem),
	 ("string?", typeTest isString),
	 ("number?" , typeTest isNumber),
	 ("symbol?", typeTest isSymbol)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = LispNumber $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (LispNumber n) = n
unpackNum _ = 0

typeTest :: (LispVal -> Bool) -> [LispVal] -> LispVal
typeTest op params = LispBool $ recursiveTypeTest op params

recursiveTypeTest :: (LispVal -> Bool) -> [LispVal] -> Bool
recursiveTypeTest op params =
	if length params == 1
		then op (head params)
	else
		op (head params) && recursiveTypeTest op (tail params)

isString :: LispVal -> Bool
isString (LispString param) = True
isString _ = False

isNumber :: LispVal -> Bool
isNumber (LispNumber param) = True
isNumber _ = False

isSymbol :: LispVal -> Bool
isSymbol (LispAtom param) = True
isSymbol _ = False
apply :: String -> [LispVal] -> LispVal
apply func args = maybe (LispBool False) ($ args) $ lookup func primitives

eval :: LispVal -> LispVal
eval val@(LispString _) = val
eval val@(LispNumber _) = val
eval val@(LispBool _) = val
eval (LispList [LispAtom "quote", val]) = val
eval (LispList (LispAtom func : args)) = apply func $ map eval args

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

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal
