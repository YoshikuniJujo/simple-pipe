
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Data.Pipe
import Data.Pipe.TChan
import Data.Pipe.IO
import System.IO

main :: IO ()
main = do
	a <- atomically newTChan
	b <- atomically newTChan
	atomically $ writeTChan a 'a'
	forkIO . forever $ threadDelay 750000 >> atomically (writeTChan b 'b')
	forkIO . (>> return ()) . runPipe $ fromTChans [a, b]
		=$= debug
		=$= (mapMonad (flush stdout . (threadDelay 500000 >>)) $
			toTChan a :: Pipe Char () IO ())
	threadDelay 5000000

flush :: Handle -> IO a -> IO a
flush h io = do { x <- io; hFlush h; return x }
