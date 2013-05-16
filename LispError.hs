module LispError (
	LispError(
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

import LispVal

import Control.Monad.Error
import Text.ParserCombinators.Parsec

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
