{-# LANGUAGE FlexibleContexts, RankNTypes, PackageImports #-}

module Data.Pipe ( PipeClass(..), Pipe, finally, bracket ) where

import Control.Applicative
import Control.Monad
import Control.Exception.Lifted (onException)
import Control.Monad.Trans.Control
import "monads-tf" Control.Monad.Trans

class PipeClass p where
	runPipe :: Monad m => p i o m r -> m (Maybe r)
	(=$=) :: Monad m => p a b m x -> p b c m y -> p a c m y
	yield :: Monad m => o -> p i o m ()
	await :: Monad m => p i o m (Maybe i)
	onBreak :: Monad m => p i o m r -> m b -> p i o m r
	onDone :: Monad m => p i o m r -> m b -> p i o m r
	finalize :: Monad m => p i o m r -> m b -> p i o m r
	mapMonad :: Monad m => (forall a . m a -> m a) -> p i o m r -> p i o m r

	p `finalize` f = p `onBreak` f `onDone` f

data Pipe i o m r
	= Ready (m ()) o (Pipe i o m r)
	| Need (m ()) (Maybe i -> Pipe i o m r)
	| Done (m ()) r
	| Make (m ()) (m (Pipe i o m r))

finalizer :: Pipe i o m r -> m ()
finalizer (Ready f _ _) = f
finalizer (Need f _) = f
finalizer (Done f _) = f
finalizer (Make f _) = f

instance PipeClass Pipe where

	runPipe (Done f r) = f >> return (Just r)
	runPipe (Make _ m) = runPipe =<< m
	runPipe _ = return Nothing

	p =$= Make f m = Make f $ (p =$=) `liftM` m
	p =$= Done f r = Done (finalizer p >> f) r
	p =$= Ready f o p' = Ready f o $ p =$= p'
	Need f n =$= p = Need f $ \i -> n i =$= p
	Ready _ o p =$= Need _ n = p =$= n (Just o)
	Done f r =$= Need f' n =
		Done (return ()) r =$= Make f' (f >> return (n Nothing))
	Make f m =$= p = Make f $ (=$= p) `liftM` m

	yield x = Ready (return ()) x (return ())
	await = Need (return ()) return

	onBreak (Ready f0 o p) f = Ready (f0 >> f >> return ()) o $ onBreak p f
	onBreak (Need f0 n) f = Need (f0 >> f >> return ()) $ \i -> onBreak (n i) f
	onBreak (Done f0 r) _ = Done f0 r
	onBreak (Make f0 m) f = Make (f0 >> f >> return ()) $ flip onBreak f `liftM` m

	onDone (Ready f0 o p) f = Ready (voidM f0) o $ finalize p f
	onDone (Need f0 n) f = Need (voidM f0) $ \i -> finalize (n i) f
	onDone (Done f0 r) f = Done (f0 >> f >> return ()) r
	onDone (Make f0 m) f = Make (voidM f0) $ flip finalize f `liftM` m

	finalize (Ready f0 o p) f = Ready (f0 >> f >> return ()) o $ finalize p f
	finalize (Need f0 n) f = Need (f0 >> f >> return ()) $ \i -> finalize (n i) f
	finalize (Done f0 r) f = Done (f0 >> f >> return ()) r
	finalize (Make f0 m) f = Make (f0 >> f >> return ()) $ flip finalize f `liftM` m

	mapMonad k (Ready f o p) = Ready f o $ mapMonad k p
	mapMonad k (Need f n) = Need f $ \i -> mapMonad k $ n i
	mapMonad _ (Done f r) = Done f r
	mapMonad k (Make f m) = Make f . k $ mapMonad k `liftM` m

instance Monad m => Monad (Pipe i o m) where
	Ready f o p >>= k = Ready f o $ p >>= k
	Need f n >>= k = Need f $ n >=> k
--	Done f r >>= k = Make (return ()) $ f >> return (k r)
	Done _ r >>= k = k r
	Make f m >>= k = Make f $ (>>= k) `liftM` m
	return = Done (return ())

instance Monad m => Functor (Pipe i o m) where
	fmap = (=<<) . (return .)

instance Monad m => Applicative (Pipe i o m) where
	pure = return
	(<*>) = ap

instance MonadTrans (Pipe i o) where
	lift = liftP

instance MonadIO m => MonadIO (Pipe i o m) where
	liftIO = lift . liftIO

liftP :: Monad m => m a -> Pipe i o m a
liftP m = Make (return ()) $ Done (return ()) `liftM` m

bracket :: (MonadBaseControl IO m, PipeClass p, MonadTrans (p i o), Monad (p i o m)) =>
	m a -> (a -> m b) -> (a -> p i o m r) -> p i o m r
bracket o c p = do
	h <- lift o
	p h `finally` void (c h)

finally :: (MonadBaseControl IO m, PipeClass p) => p i o m r -> m b -> p i o m r
finally p f = finalize (mapMonad (`onException` f) p) f

voidM :: Monad m => m a -> m ()
voidM = (>> return ())
