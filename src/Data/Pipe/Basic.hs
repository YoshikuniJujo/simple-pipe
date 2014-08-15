module Data.Pipe.Basic (convert) where

import Data.Pipe

convert :: Monad m => (a -> b) -> Pipe a b m ()
convert f = await >>= maybe (return ()) ((>> convert f) . yield . f)
