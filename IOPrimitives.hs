module IOPrimitives (
	ioPrimitives
) where

import IError
import LispVal

import Control.Monad.Error
import System.IO

ioPrimitives :: [(String, [LispVal] -> IOThrowsLispError LispVal)]
ioPrimitives =
	[("apply", applyProc),
	 ("open-input-file", makePort ReadMode),
	 ("open-output-file", makePort WriteMode),
	 ("close-input-port", closePort),
	 ("close-output-port", closePort),
	 ("read", readProc),
	 ("write", writeProc),
	 ("read-contents", readContents),
	 ("read-all", readAll)]

applyProc :: [LispVal] -> IOThrowsLispError LispVal
applyProc (func, LispList args) = apply func args
applyProc (func : args) = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsLispError LispVal
makePort mode [LispString filename] =
	liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsLispError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ LispBool True)
closePort _ = return $ LispBool False

readProc :: [LispVal] -> IOThrowsLispError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsLispError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return LispBool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [LispString filename] =
	liftM LispString $ liftIO $ readFile filename

load :: String -> IOThrowsLispError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [LispString filename] = liftM LispList $ load filename
