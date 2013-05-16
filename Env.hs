module Env (
	Env,
	nullEnv,
	setVar,
	getVar,
	defineVar
) where

import LispError
import LispVal

import Control.Monad
import Control.Monad.Error
import Data.IORef
import System.IO

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

-- Check if a variable has been bound
isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>=
	return . maybe False (const True) . lookup var

-- Get the value of a variable
getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
	env <- liftIO $ readIORef envRef
	maybe
		(throwError $ UnboundVar "Getting an unbound variable" var)
		(liftIO . readIORef)
		(lookup var env)

-- Set the value of a variable
setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var val = do
	env <- liftIO $ readIORef envRef
	maybe
		(throwError $ UnboundVar "Setting an unbound variable" var)
		(liftIO . (flip writeIORef val))
		(lookup var env)
	return val

-- Define a new variable
defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var val = do
	alreadyDefined <- liftIO $ isBound envRef var
	if alreadyDefined
		then setVar envRef var val >> return val
		else liftIO $ do
			valRef <- newIORef val
			env <- readIORef envRef
			writeIORef envRef ((var, valRef) : env)
			return val

-- Bind multiple variables
bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
	where
		extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
		addBinding (var, val) = do
			ref <- newIORef val
			return (var, ref)
