module Data.Pipe.Basic (convert) where

import Data.Pipe

convert :: (PipeClass p, Monad m, Monad (p a b m)) => (a -> b) -> p a b m ()
convert f = await >>= maybe (return ()) ((>> convert f) . yield . f)
