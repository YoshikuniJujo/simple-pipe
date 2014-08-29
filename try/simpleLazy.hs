import Control.Applicative
import System.IO.Unsafe

data Lazy o
	= Ready o (Lazy o)
	| Make (IO (Lazy o))
	| Done

example :: Lazy Int
example = Ready 8 $ Make (print 111 >> return (Ready 111 $ Make $ return Done))

toStrict :: Lazy o -> IO [o]
toStrict (Ready x l) = (x :) <$> toStrict l
toStrict (Make l) = l >>= toStrict
toStrict Done = return []

toLazy :: Int -> Lazy o -> IO [o]
toLazy 0 _ = return []
toLazy n (Ready x l) = (x :) <$> unsafeInterleaveIO (toLazy' (n - 1) l)
toLazy n (Make l) = unsafeInterleaveIO l >>= toLazy' (n - 1)
toLazy n Done = return []

toLazy' :: Int -> Lazy o -> IO [o]
toLazy' 0 _ = return []
toLazy' n (Make l) = unsafeInterleaveIO l >>= toLazy (n - 1)
toLazy' n (Ready x l) = (x :) <$> toLazy (n - 1) l
toLazy' n Done = return []

sample :: IO [Int]
sample = unsafeInterleaveIO $ do
	print 1
	(1 :) <$> sample
