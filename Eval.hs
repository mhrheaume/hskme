module Eval (
	eval
) where

import Lisp
import Primitives

import Control.Monad.Error

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params vargs body closure) args =
	if num params /= num args && vargs == Nothing
		then throwError $ NumArgs (num params) args
		else bindParams >>= bindVarArgs vargs >>= evalBody
	where
		restArgs = drop (length params) args
		num = toInteger . length
		bindParams = liftIO $ bindVars closure $ zip params args
		bindVarArgs arg env = case arg of
			Just argn -> liftIO $ bindVars env [(argn, LispList $ restArgs)]
			Nothing -> return env
		evalBody env = liftM last $ mapM (eval env) body


evalIf :: LispEnv -> LispVal -> LispVal -> LispVal -> IOThrowsError LispVal
evalIf env pred conseq alt = do
	result <- eval env pred
	case result of
		LispBool True -> eval env conseq
		LispBool False -> eval env alt
		otherwise -> throwError $ TypeMismatch "boolean" result

lastVal :: [LispVal] -> IOThrowsError LispVal
lastVal args = return $ last args

evalCond :: LispEnv -> LispVal -> IOThrowsError LispVal
-- This is unspecified: return false for now
evalCond env (LispList []) = return $ LispBool False
evalCond env (LispList (x : xs)) = evalClause x
	where
		evalClause (LispList [pred]) = do
			result <- eval env pred
			case result of
				LispBool True -> return result
				LispBool False -> evalCond env $ LispList xs
				otherwise -> throwError $ TypeMismatch "boolean" result
		evalClause (LispList (LispAtom "else" : rest)) =
			mapM (eval env) rest >>= lastVal
		evalClause (LispList (pred : rest)) = do
			result <- eval env pred
			case result of
				LispBool True -> mapM (eval env) rest >>= lastVal
				LispBool False -> evalCond env $ LispList xs
				otherwise -> throwError $ TypeMismatch "boolean" result
		evalClause other = throwError $ BadSpecialForm "malformed cond clause" other

checkDatum :: LispVal -> LispVal -> IOThrowsError LispVal
checkDatum key (LispList []) = return $ LispBool False
checkDatum key (LispList (x : xs)) = do
	result <- liftThrows $ eqv [key, x]
	case result of
		LispBool True -> return $ LispBool True
		LispBool False -> checkDatum key $ LispList xs

evalCase :: LispEnv -> LispVal -> LispVal -> IOThrowsError LispVal
-- This is unspcified: return false for now
evalCase env (LispList []) key = return $ LispBool False
evalCase env (LispList (x : xs)) key = evalClause x
	where
		evalClause (LispList (LispAtom "else" : rest)) =
			mapM (eval env) rest >>= lastVal
		evalClause (LispList (datum : rest)) = do
			result <- checkDatum key datum
			case result of
				LispBool True -> mapM (eval env) rest >>= lastVal
				LispBool False -> evalCase env (LispList xs) key
		evalClause other = throwError $ BadSpecialForm "malformed case clause" other

eval :: LispEnv -> LispVal -> IOThrowsError LispVal
eval env val@(LispString _) = return val
eval env val@(LispNumber _) = return val
eval env val@(LispBool _) = return val
eval env (LispAtom id) = getVar env id
eval env (LispList [LispAtom "quote", val]) = return val

-- Define / set variables
eval env (LispList [LispAtom "set!", LispAtom var, form]) =
	eval env form >>= setVar env var
eval env (LispList [LispAtom "define", LispAtom var, form]) =
	eval env form >>= defineVar env var

-- Conditionals
eval env (LispList [LispAtom "if", pred, conseq, alt]) = evalIf env pred conseq alt
eval env (LispList (LispAtom "cond" : clauses)) = evalCond env $ LispList clauses
eval env (LispList (LispAtom "case" : key : clauses)) =
	eval env key >>= (evalCase env $ LispList clauses)

-- Function definitions
eval env (LispList (LispAtom "define" : LispList (LispAtom var : params) : body)) =
	makeNormalFunc env params body >>= defineVar env var
eval env (LispList (LispAtom "define" : LispDottedList (LispAtom var : params) vargs : body)) =
	makeVargsFunc vargs env params body >>= defineVar env var
eval env (LispList (LispAtom "lambda" : LispList params : body)) =
	makeNormalFunc env params body
eval env (LispList (LispAtom "lambda" : LispDottedList params vargs : body)) =
	makeVargsFunc vargs env params body
eval env (LispList (LispAtom "lambda" : vargs@(LispAtom _) : body)) =
	makeVargsFunc vargs env [] body

-- Function calls
eval env (LispList (func : args)) = do
	lookup <- eval env func
	argVals <- mapM (eval env) args
	apply lookup argVals

-- Error
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm
