module Environment (
	Environment,
	nullEnvironment,
	setVar,
	getVar,
	defineVar,
	bindVars,
) where

import IError

import Control.Monad
import Control.Monad.Error
import Data.IORef
import System.IO

type Environment a = IORef [(String, IORef a)]

nullEnvironment :: IO (Environment a)
nullEnvironment = newIORef []

-- Check if a variable has been bound
isBound :: (Environment a) -> String -> IO Bool
isBound envRef var = readIORef envRef >>=
	return . maybe False (const True) . lookup var

-- Get the value of a variable
getVar :: (Environment a) -> String -> IOThrowsError a
getVar envRef var = do
	env <- liftIO $ readIORef envRef
	maybe
		(throwError $ UnboundVar "Getting an unbound variable" var)
		(liftIO . readIORef)
		(lookup var env)

-- Set the value of a variable
setVar :: (Environment a) -> String -> a -> IOThrowsError a
setVar envRef var val = do
	env <- liftIO $ readIORef envRef
	maybe
		(throwError $ UnboundVar "Setting an unbound variable" var)
		(liftIO . (flip writeIORef val))
		(lookup var env)
	return val

-- Define a new variable
defineVar :: (Environment a) -> String -> a -> IOThrowsError a
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
bindVars :: (Environment a) -> [(String, a)] -> IO (Environment a)
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
	where
		extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
		addBinding (var, val) = do
			ref <- newIORef val
			return (var, ref)

