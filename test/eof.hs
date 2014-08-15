{-# LANGUAGE ScopedTypeVariables #-}

import System.IO
import Control.Exception
import GHC.IO.Exception

main :: IO ()
main = do
	h <- openFile "test/sample.txt" ReadMode
	hGetLine h >>= putStrLn
	hGetLine h >>= putStrLn
	(hGetLine h >>= putStrLn) `catch` \(e :: IOException) -> do
		print e
		print $ ioe_type e
		print EOF
		case ioe_type e of
			EOF -> return ()
			_ -> ioError e
	putStrLn "hello"
