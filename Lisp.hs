module Lisp (
	-- FIXME: Separate error, env, and val into separate modules. Right now
	-- they are all dependent on eachother.

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

	-- LispError
	LispError (
		NumArgs,
		TypeMismatch,
		Parser,
		BadSpecialForm,
		NotFunction,
		UnboundVar,
		Default
	),
	ThrowsError,
	IOThrowsError,
	trapError,
	extractValue,
	liftThrows,
	runIOThrows,

	-- LispEnv
	LispEnv,
	nullEnv,
	setVar,
	getVar,
	defineVar,
	bindVars,
	makeNormalFunc,
	makeVargsFunc
) where

import Control.Monad
import Control.Monad.Error
import Data.IORef
import Text.ParserCombinators.Parsec
import System.IO

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
				, closure :: LispEnv
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

data LispError = NumArgs Integer [LispVal]
			   | TypeMismatch String LispVal
			   | Parser ParseError
			   | BadSpecialForm String LispVal
			   | NotFunction String String
			   | UnboundVar String String
			   | Default String

type ThrowsError = Either LispError
type IOThrowsError = ErrorT LispError IO

showError :: LispError -> String
showError (NumArgs n vals) =
	"Expected " ++ show n ++ " args; found values " ++ unwordsList vals
showError (TypeMismatch expect found) =
	"Invalid type: expected " ++ expect ++ "; found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (UnboundVar message varname) = message ++ ": " ++ varname

trapError :: (Show e, MonadError e m) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

instance Show LispError where show = showError
instance Error LispError where
	noMsg = Default "An error has occured"
	strMsg = Default

type LispEnv = IORef [(String, IORef LispVal)]

nullEnv :: IO (LispEnv)
nullEnv = newIORef []

-- Check if a variable has been bound
isBound :: LispEnv -> String -> IO Bool
isBound envRef var = readIORef envRef >>=
	return . maybe False (const True) . lookup var

-- Get the value of a variable
getVar :: LispEnv -> String -> IOThrowsError LispVal
getVar envRef var = do
	env <- liftIO $ readIORef envRef
	maybe
		(throwError $ UnboundVar "Getting an unbound variable" var)
		(liftIO . readIORef)
		(lookup var env)

-- Set the value of a variable
setVar :: LispEnv -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var val = do
	env <- liftIO $ readIORef envRef
	maybe
		(throwError $ UnboundVar "Setting an unbound variable" var)
		(liftIO . (flip writeIORef val))
		(lookup var env)
	return val

-- Define a new variable
defineVar :: LispEnv -> String -> LispVal -> IOThrowsError LispVal
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
bindVars :: LispEnv -> [(String, LispVal)] -> IO (LispEnv)
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
	where
		extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
		addBinding (var, val) = do
			ref <- newIORef val
			return (var, ref)

makeFunc :: (Maybe String) -> LispEnv -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc vargs env params body = return $ Func (map showVal params) vargs body env

makeNormalFunc = makeFunc Nothing
makeVargsFunc = makeFunc . Just . showVal

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

