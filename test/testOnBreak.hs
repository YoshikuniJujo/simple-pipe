{-# LANGUAGE PackageImports #-}

import Control.Monad
import "monads-tf" Control.Monad.Trans
import Data.Pipe
import Data.Pipe.List

input :: Pipe () Char IO ()
input = fromList "Hello, world!" `onBreak` putStrLn "finalize"

fromStdin :: Pipe () String IO ()
fromStdin = do
	l <- lift getLine
	unless (null l) $ yield l >> fromStdin

takeP :: Monad m => Int -> Pipe a a m ()
takeP 0 = return ()
takeP n = do
	mx <- await
	case mx of
		Just x -> yield x >> takeP (n - 1)
		_ -> return ()

output :: Pipe Char () IO String
output = toList
