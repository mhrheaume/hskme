module Eval (
	eval
) where

import LispError
import LispVal
import Primitives

import Control.Monad.Error

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe
	(throwError $ NotFunction "Unrecognized primitive function args" func)
	($ args)
	(lookup func primitives)

evalIf :: LispVal -> LispVal -> LispVal -> ThrowsError LispVal
evalIf pred conseq alt = do
	result <- eval pred
	case result of
		LispBool True -> eval conseq
		LispBool False -> eval alt
		otherwise -> throwError $ TypeMismatch "boolean" result

lastVal :: [LispVal] -> ThrowsError LispVal
lastVal args = return $ last args

evalCond :: LispVal -> ThrowsError LispVal
-- This is unspecified: return false for now
evalCond (LispList []) = return $ LispBool False
evalCond (LispList (x : xs)) = evalClause x
	where
		evalClause (LispList [pred]) = do
			result <- eval pred
			case result of
				LispBool True -> return result
				LispBool False -> evalCond $ LispList xs
				otherwise -> throwError $ TypeMismatch "boolean" result
		evalClause (LispList (LispAtom "else" : rest)) = mapM eval rest >>= lastVal
		evalClause (LispList (pred : rest)) = do
			result <- eval pred
			case result of
				LispBool True -> mapM eval rest >>= lastVal
				LispBool False -> evalCond $ LispList xs
				otherwise -> throwError $ TypeMismatch "boolean" result
		evalClause other = throwError $ BadSpecialForm "malformed cond clause" other

checkDatum :: LispVal -> LispVal -> ThrowsError LispVal
checkDatum key (LispList []) = return $ LispBool False
checkDatum key (LispList (x : xs)) = do
	result <- eqv [key, x]
	case result of
		LispBool True -> return $ LispBool True
		LispBool False -> checkDatum key $ LispList xs

evalCase :: LispVal -> LispVal -> ThrowsError LispVal
-- This is unspcified: return false for now
evalCase (LispList []) key = return $ LispBool False
evalCase (LispList (x : xs)) key = evalClause x
	where
		evalClause (LispList (LispAtom "else" : rest)) = mapM eval rest >>= lastVal
		evalClause (LispList (datum : rest)) = do
			result <- checkDatum key datum
			case result of
				LispBool True -> mapM eval rest >>= lastVal
				LispBool False -> evalCase (LispList xs) key
		evalClause other = throwError $ BadSpecialForm "malformed case clause" other

eval :: LispVal -> ThrowsError LispVal
eval val@(LispString _) = return val
eval val@(LispNumber _) = return val
eval val@(LispBool _) = return val
eval (LispList [LispAtom "quote", val]) = return val
eval (LispList [LispAtom "if", pred, conseq, alt]) = evalIf pred conseq alt
eval (LispList (LispAtom "cond" : clauses)) = evalCond $ LispList clauses
eval (LispList (LispAtom "case" : key : clauses)) = eval key >>= (evalCase $ LispList clauses)
eval (LispList (LispAtom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm
