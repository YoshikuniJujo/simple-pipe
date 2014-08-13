{-# LANGUAGE FlexibleContexts #-}

import PipeF
import Control.Monad
import System.IO

readStdin :: Pipe IO () Char ()
readStdin = do
	c <- liftP getChar
	liftP $ print c
	case c of
		'q' -> return ()
		_ -> yield c >> readStdin

toList :: Monad m => Pipe m a () [a]
toList = await >>= \mx -> case mx of
	Just x -> (x :) `liftM` toList
	_ -> return []

takeN :: Monad m => Int -> Pipe m a () [a]
takeN 0 = return []
takeN n = do
	mx <- await
	case mx of
		Just x -> (x :) `liftM` takeN (n - 1)
		_ -> return []

take1 :: Monad m => Pipe m a () (Maybe a)
take1 = await

readf :: FilePath -> Pipe IO () String ()
readf fp = bracketP
	(openFile fp ReadMode) (\h -> putStrLn "finalize" >> hClose h) hRead

hRead :: Handle -> Pipe IO () String ()
hRead h = do
	eof <- liftP $ hIsEOF h
	unless eof $ do
		l <- liftP $ hGetLine h
		yield l
		hRead h

takeP :: Monad m => Int -> Pipe m [a] [a] ()
takeP 0 = return ()
takeP n = do
	mx <- await
	case mx of
		Just x -> yield x >> takeP (n - 1)
		_ -> return ()

writeString :: Pipe IO String () ()
writeString = do
	s <- await
	maybe (return ()) ((>> writeString) . liftP . putStrLn) s
