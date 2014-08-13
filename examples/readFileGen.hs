{-# LANGUAGE FlexibleContexts, PackageImports #-}

import Data.Pipe
import Control.Monad
import System.IO

import "monads-tf" Control.Monad.Trans

readStdin :: Pipe () Char IO ()
readStdin = do
	c <- lift getChar
	lift $ print c
	case c of
		'q' -> return ()
		_ -> yield c >> readStdin

toList :: Monad m => Pipe a () m [a]
toList = await >>= \mx -> case mx of
	Just x -> (x :) `liftM` toList
	_ -> return []

takeN :: Monad m => Int -> Pipe a () m [a]
takeN 0 = return []
takeN n = do
	mx <- await
	case mx of
		Just x -> (x :) `liftM` takeN (n - 1)
		_ -> return []

take1 :: Monad m => Pipe a () m (Maybe a)
take1 = await

readf :: FilePath -> Pipe () String IO ()
readf fp = bracket
	(openFile fp ReadMode) (\h -> putStrLn "finalize" >> hClose h) hRead

hRead :: Handle -> Pipe () String IO ()
hRead h = do
	eof <- lift $ hIsEOF h
	unless eof $ do
		l <- lift $ hGetLine h
		yield l
		hRead h

takeP :: Monad m => Int -> Pipe [a] [a] m ()
takeP 0 = return ()
takeP n = do
	mx <- await
	case mx of
		Just x -> yield x >> takeP (n - 1)
		_ -> return ()

writeString :: Pipe String () IO ()
writeString = do
	s <- await
	maybe (return ()) ((>> writeString) . lift . putStrLn) s
