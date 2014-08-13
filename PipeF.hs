{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module PipeF (
	PipeClass(..), Pipe, runPipe, liftP, yield, await, finalize, finalize',
	bracketP,
	) where

import Control.Monad
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Exception

class PipeClass p where
	(=$=) :: p a b x -> p b c y -> p a c y

data Pipe m i o r
	= Ready (m ()) o (Pipe m i o r)
	| Need (m ()) (Maybe i -> Pipe m i o r)
	| Done (m ()) r
	| Make (m ()) (m (Pipe m i o r))

finalizer :: Pipe m i o r -> m ()
finalizer (Ready f _ _) = f
finalizer (Need f _) = f
finalizer (Done f _) = f
finalizer (Make f _) = f

instance Monad m => PipeClass (Pipe m) where
	p =$= Done f r = Done (finalizer p >> f) r
	p =$= Ready f o p' = Ready f o $ p =$= p'
	Need f n =$= p = Need f $ \i -> n i =$= p
	Ready _ o p =$= Need _ n = p =$= n (Just o)
	Done f r =$= Need f' n =
		Done (return ()) r =$= Make f' (f >> return (n Nothing))
	Make f m =$= p = Make f $ (=$= p) `liftM` m
	p =$= Make f m = Make f $ (p =$=) `liftM` m

instance Monad m => Monad (Pipe m i o) where
	Ready f o p >>= k = Ready f o $ p >>= k
	Need f n >>= k = Need f $ n >=> k
--	Done f r >>= k = Make (return ()) $ f >> return (k r)
	Done _ r >>= k = k r
	Make f m >>= k = Make f $ (>>= k) `liftM` m
	return = Done (return ())

runPipe :: Monad m => Pipe m i o r -> m (Maybe r)
runPipe (Done f r) = f >> return (Just r)
runPipe (Make _ m) = runPipe =<< m
runPipe _ = return Nothing

liftP :: Monad m => m a -> Pipe m i o a
liftP m = Make (return ()) $ Done (return ()) `liftM` m

yield :: Monad m => o -> Pipe m i o ()
yield x = Ready (return ()) x (return ())

await :: Monad m => Pipe m i o (Maybe i)
await = Need (return ()) return

bracketP :: (MonadIO m, MonadBase m IO) =>
	m a -> (a -> m b) -> (a -> Pipe m i o r) -> Pipe m i o r
bracketP o c p = do
	h <- liftP o
	p h `finalize'` void (c h)

finalize :: Monad m => Pipe m i o r -> m () -> Pipe m i o r
finalize (Ready _ o p) f = Ready f o $ finalize p f
finalize (Need _ n) f = Need f $ \i -> finalize (n i) f
finalize (Done _ r) f = Done f r
finalize (Make _ m) f = Make f $ flip finalize f `liftM` m

finalize' :: (MonadIO m, MonadBase m IO) => Pipe m i o r -> m () -> Pipe m i o r
finalize' p f =
	finalize (mapMake (liftIO . (`onException` liftBase f) . liftBase) p) f

mapMake :: Monad m => (forall a . m a -> m a) -> Pipe m i o r -> Pipe m i o r
mapMake k (Ready f o p) = Ready f o $ mapMake k p
mapMake k (Need f n) = Need f $ \i -> mapMake k $ n i
mapMake _ (Done f r) = Done f r
mapMake k (Make f m) = Make f . k $ mapMake k `liftM` m
