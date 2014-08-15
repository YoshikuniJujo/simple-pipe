{-# LANGUAGE TypeFamilies, PackageImports, ScopedTypeVariables #-}

import "monads-tf" Control.Monad.Error
import Data.Pipe
import Data.Pipe.IO
import System.IO

import Control.Exception

main :: IO ()
main = do
	r <- runErrorT . runPipe $ pipe `catchError` \(e :: String) -> do
		lift . lift $ print e
		pipe
	print r

pipe :: (MonadIO m, MonadError m, ErrorType m ~ String) => Pipe () () m ()
pipe = fromHandle stdin
	=$= process
	=$= toHandle stdout

process :: (MonadError m, ErrorType m ~ String) => Pipe Char Char m ()
process = await >>= \mx -> case mx of
	Just 'w' -> throwError "HOHOHO" >> process
	Just x -> yield x >> process
	_ -> return ()
