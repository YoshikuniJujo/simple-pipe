module Data.Pipe.Flow (before, until) where

import Prelude hiding (until)
import Control.Monad
import Data.Pipe

before :: (PipeClass p, Monad m, Monad (p a a m)) => (a -> Bool) -> p a a m ()
before p = await >>= maybe (return ()) (\x -> unless (p x) $ yield x >> before p)

until :: (PipeClass p, Monad m, Monad (p a a m)) => (a -> Bool) -> p a a m ()
until p = (await >>=) . maybe (return ()) $ \x ->
	if p x then yield x else yield x >> before p
