module IError (
	IError (
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
	runIOThrows
) where

import Util

import Control.Monad
import Control.Monad.Error
import Text.ParserCombinators.Parsec
import System.IO

data IError a = NumArgs Integer [a]
			   | TypeMismatch String a
			   | Parser ParseError
			   | BadSpecialForm String a
			   | NotFunction String String
			   | UnboundVar String String
			   | Default String

type ThrowsError a = Either (IError a) a
type IOThrowsError a = ErrorT (IError a) IO a

showError :: (Show a) => IError a -> String
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

instance (Show a) => Show (IError a) where show = showError
instance Error (IError a) where
	noMsg = Default "An error has occured"
	strMsg = Default

