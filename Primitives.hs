module Primitives (
	primitives,
	eqv
) where

import LispError
import LispVal

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
	 ("cons", cons),
	 ("eq?", eqv),
	 ("eqv?", eqv)]

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

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(LispBool x1), (LispBool x2)] = return $ LispBool $ x1 == x2
eqv [(LispNumber x1), (LispNumber x2)] = return $ LispBool $ x1 == x2
eqv [(LispString x1), (LispString x2)] = return $ LispBool $ x1 == x2
eqv [(LispAtom x1), (LispAtom x2)] = return $ LispBool $ x1 == x2
eqv [(LispDottedList xs x), (LispDottedList ys y)] =
	eqv [LispList $ xs ++ [x], LispList $ ys ++ [y]]
eqv [(LispList x1), (LispList x2)] =
	return $ LispBool $ (length x1 == length x2) && (all eqvPair $ zip x1 x2)
	where
		eqvPair (x1, x2) = case eqv [x1, x2] of
			Left err -> False
			Right (LispBool val) -> val
eqv [_, _] = return $ LispBool False
eqv other = throwError $ NumArgs 2 other