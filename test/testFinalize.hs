import Data.Pipe
import Data.Pipe.List

input :: Pipe () Char IO ()
input = fromList "Hello, world!" `finalize` putStrLn "finalize"

takeP :: Monad m => Int -> Pipe a a m ()
takeP 0 = return ()
takeP n = do
	mx <- await
	case mx of
		Just x -> yield x >> takeP (n - 1)
		_ -> return ()

output :: Pipe Char () IO String
output = toList
