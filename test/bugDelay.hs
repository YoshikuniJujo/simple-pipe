{-# LANGUAGE PackageImports #-}

import "monads-tf" Control.Monad.Trans
import Data.Pipe

-- idP :: Monad m => Pipe a a m ()
-- idP = await >>= maybe (return ()) (\x -> yield x >> idP)
idP = await >>= \mx -> case mx of
	Just x -> do
		lift $ print x
		yield x
		idP
	_ -> return ()

fromStdin :: Pipe () String IO ()
fromStdin = do
	l <- lift getLine
	yield l
	fromStdin

toStdout :: Pipe String () IO ()
toStdout = await >>= maybe (return ()) (\l -> lift (putStrLn l) >> toStdout)

normal :: IO (Maybe ())
normal = runPipe $ fromStdin =$= toStdout

bad :: IO (Maybe ())
bad = runPipe $ fromStdin =$= (idP =$= toStdout)
