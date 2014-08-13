import Pipeline
import Control.Monad
import System.IO

readStdin :: Pipeline IO () Char ()
readStdin = do
	c <- liftP getChar
	case c of
		'q' -> return ()
		_ -> do	yield c
			readStdin

toList :: Monad m => Pipeline m a () [a]
toList = do
	mx <- await
	case mx of
		Just x -> (x :) `liftM` toList
		_ -> return []

takeN :: Monad m => Int -> Pipeline m a () [a]
takeN 0 = return []
takeN n = do
	mx <- await
	case mx of
		Just x -> (x :) `liftM` takeN (n - 1)
		_ -> return []

test :: IO (Maybe String)
test = runPipe . (`finalize` putStrLn "finalize") $ readStdin =$= toList

test2 :: IO (Maybe String)
test2 = runPipe . (`finalize` putStrLn "finalize") $ readStdin =$= takeN 5

test3 :: IO (Maybe String)
test3 = runPipe . (`finalize'` putStrLn "finalize") $ readStdin =$= takeN 5

{-
readfPl :: FilePath -> Pipe IO () String ()
readfPl fp = do
	h <- liftP $ openFile fp ReadMode
	-}

hRead :: Handle -> Pipeline IO () String ()
hRead h = do
	l <- liftP $ hGetLine h
	yield l
	hRead h
