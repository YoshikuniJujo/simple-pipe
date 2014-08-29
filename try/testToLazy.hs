{-# LANGUAGE PackageImports #-}

-- import Prelude hiding (take)

import Control.Applicative
import "monads-tf" Control.Monad.Trans
import Data.Pipe
import Data.Pipe.Lazy
import Data.Pipe.List
import Data.Pipe.ByteString hiding (toLazy)
import qualified Data.Pipe.ByteString as PBS
import Data.Time
import System.IO
import System.IO.Unsafe

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS

timePipe :: Pipe () UTCTime IO ()
timePipe = lift getCurrentTime >>= yield >> timePipe

{-
take :: Monad m => Int -> Pipe a a m ()
take 0 = return ()
take n = await >>= maybe (return ()) ((>> take (n - 1)) . yield)
-}

hGetLines :: Handle -> IO [String]
hGetLines h = unsafeInterleaveIO $ (:) <$> hGetLine h <*> hGetLines h

hGetLines' :: Handle -> IO [String]
hGetLines' h = hGetLines h >>=
	(toLazy :: Pipe () String IO () -> IO [String])  . fromList

bsHGetLine :: Handle -> IO LBS.ByteString
bsHGetLine = PBS.toLazy . (fromHandleLn :: Handle -> Pipe () BSC.ByteString IO ())
