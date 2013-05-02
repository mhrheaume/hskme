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
	 ("remainder", numericBinop rem),
	 ("=", numBoolBinop (==)),
	 ("<", numBoolBinop (<)),
	 (">", numBoolBinop (>)),
	 ("/=", numBoolBinop (/=)),
	 (">=", numBoolBinop (>=)),
	 ("<=", numBoolBinop (<=)),
	 ("&&", boolBoolBinop (&&)),
	 ("||", boolBoolBinop (||)),
	 ("string=?", strBoolBinop (==)),
	 ("string<?", strBoolBinop (<)),
	 ("string>?", strBoolBinop (>)),
	 ("string<=?", strBoolBinop (<=)),
	 ("string>=?", strBoolBinop (>=)),
	 ("car", car),
	 ("cdr", cdr),
	 ("cons", cons)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . LispNumber . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args =
	if length args /= 2
	then throwError $ NumArgs 2 args
	else do
		left <- unpacker $ args !! 0
		right <- unpacker $ args !! 1
		return $ LispBool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (LispNumber n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (LispString s) = return s
unpackStr (LispNumber s) = return $ show s
unpackStr (LispBool s) = return $ show s
unpackStr other = throwError $ TypeMismatch "string" other

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (LispBool b) = return b
unpackBool other = throwError $ TypeMismatch "boolean" other

car :: [LispVal] -> ThrowsError LispVal
car [LispList (x : xs)] = return x
car [LispDottedList (x : xs) _] = return x
car [other] = throwError $ TypeMismatch "pair" other
car other = throwError $ NumArgs 1 other

cdr :: [LispVal] -> ThrowsError LispVal
cdr [LispList (x : xs)] = return $ LispList xs
cdr [LispDottedList [_] x] = return x
cdr [LispDottedList (_ : xs) x] = return $ LispDottedList xs x
cdr [other] = throwError $ TypeMismatch "pair" other
cdr other = throwError $ NumArgs 1 other

cons :: [LispVal] -> ThrowsError LispVal
cons [x, LispList []] = return $ LispList [x]
cons [x, LispList xs] = return $ LispList $ x : xs
cons [x, LispDottedList xs xend] = return $ LispDottedList (x : xs) xend
cons [x1, x2] = return $ LispDottedList [x1] x2
cons other = throwError $ NumArgs 2 other

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
eval (LispList [LispAtom "if", pred, conseq, alt]) = do
	result <- eval pred
	case result of
		LispBool False -> eval alt
		otherwise -> eval conseq
eval (LispList (LispAtom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm
