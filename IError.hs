module IError (
	IError (
		NumArgs,
		TypeMismatch,
		Parser,
		BadSpecialForm,
		NotFunction,
		UnboundVar,
		InvalidArgument,
		Default
	),
	ThrowsError,
	IOThrowsError,
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
			  | InvalidArgument Integer String
			  | Default String

type ThrowsError a b = Either (IError a) b
type IOThrowsError a b = ErrorT (IError a) IO b

showError :: (Show a) => IError a -> String
showError (NumArgs n vals) =
	"Expected " ++ show n ++ " args; found values " ++ unwordsList vals
showError (TypeMismatch expect found) =
	"Invalid type: expected " ++ expect ++ "; found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (InvalidArgument n message) = "Invalid argument " ++ show n ++ ": " ++ message

trapError :: (Show e, MonadError e m) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: (ThrowsError a b) -> b
extractValue (Right val) = val

liftThrows :: (ThrowsError a b) -> (IOThrowsError a b)
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: (Show a) => (IOThrowsError a String) -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

instance (Show a) => Show (IError a) where show = showError
instance Error (IError a) where
	noMsg = Default "An error has occured"
	strMsg = Default

