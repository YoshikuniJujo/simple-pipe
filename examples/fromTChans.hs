import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Data.Pipe
import Data.Pipe.TChan
import Data.Pipe.IO
import System.IO

runs :: [(Int, Char)]
runs = zip [125000, 250000 ..] $
	['a' .. 'z'] ++ ['A' .. 'Z'] ++ [' ' .. '@'] ++ ['[' .. '`'] ++ ['{' .. '~']

main :: IO ()
main = do
	a <- atomically newTChan
	b <- atomically newTChan
	chns <- forM runs $ \(i, chr) -> do
		chn <- atomically newTChan
		forkIO . forever $ threadDelay i >> atomically (writeTChan chn chr)
		return chn
	forkIO . (>> return ()) . runPipe $ fromTChans chns
		=$= (mapMonad (flush stdout) $ toHandle stdout
			:: Pipe Char () IO ())
	threadDelay 30000000

flush :: Handle -> IO a -> IO a
flush h io = do { x <- io; hFlush h; return x }
