{-# LANGUAGE RankNTypes, FlexibleContexts #-}

module Pipeline (
	PipelineClass(..),
	Pipe, Pipeline, runPipe, liftP, yield, await, finalize, finalize' ) where

-- import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Exception

class PipelineClass p where
	(=$=) :: p a b x -> p b c y -> p a c y

data Pipeline m i o r
	= Ready o (Pipeline m i o r)
	| Need (Maybe i -> Pipeline m i o r)
	| Done r
	| Make (m (Pipeline m i o r))

runPipeline :: Monad m => Pipeline m i o r -> m (Maybe r)
runPipeline (Done x) = return $ Just x
runPipeline (Make m) = runPipeline =<< m
runPipeline _ = return Nothing

instance Monad m => PipelineClass (Pipeline m) where
	_ =$= Done r = Done r
	Need n =$= p = Need $ \i -> n i =$= p
	p =$= Ready o n = Ready o $ p =$= n
	Ready o p =$= Need n = p =$= n (Just o)
	Done r =$= Need n = Done r =$= n Nothing
	Make m =$= p = Make $ (=$= p) `liftM` m
	p =$= Make m = Make $ (p =$=) `liftM` m

instance Monad m => Monad (Pipeline m i o) where
	Ready o p >>= f = Ready o $ p >>= f
	Need n >>= f = Need $ n >=> f
	Done r >>= f = f r
	Make m >>= f = Make $ (>>= f) `liftM` m
	return = Done

data Pipe m i o r = Pipe (m ()) (Pipeline m i o r)

instance Monad m => PipelineClass (Pipe m) where
	Pipe f1 p1@(Done _) =$= Pipe f2 p2 = Pipe f2 $ do
		r <- p1 =$= p2
		Make $ Done `liftM` f1
		return r
	Pipe f1 p1 =$= Pipe f2 p2 = Pipe (f1 >> f2) (p1 =$= p2)

-- instance Monad m => Monad (Pipe m i o) where
--	Pipe f1 (Ready o p) >>= f = 

runPipe :: Monad m => Pipe m i o r -> m (Maybe r)
runPipe (Pipe f p) = runPipeline p >>= \x -> f >> return x

liftP :: Monad m => m a -> Pipeline m i o a
liftP m = Make $ Done `liftM` m

yield :: Monad m => o -> Pipeline m i o ()
yield x = Ready x (return ())

await :: Monad m => Pipeline m i o (Maybe i)
await = Need return

finalize :: Pipeline m i o r -> m () -> Pipe m i o r
finalize p f = Pipe f p

finalize' :: (MonadIO m, MonadBase m IO) =>
	Pipeline m i o r -> IO () -> Pipe m i o r
finalize' p f = Pipe (liftIO f) $ mapMake (liftIO . (`onException` f) . liftBase) p

{-
bracketP :: (MonadIO m, MonadBase m IO) =>
	IO a -> (a -> IO b) -> (a -> Pipeline m i o r) -> Pipe m i o r
bracketP o f m = 
-}

mapMake :: Monad m => (forall a . m a -> m a) -> Pipeline m i o r -> Pipeline m i o r
mapMake f (Ready o p) = Ready o $ mapMake f p
mapMake f (Need n) = Need $ \i -> mapMake f $ n i
mapMake _ (Done r) = Done r
mapMake f (Make m) = Make . f $ mapMake f `liftM` m
