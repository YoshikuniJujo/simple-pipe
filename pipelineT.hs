{-# LANGUAGE KindSignatures #-}

import Control.Applicative
import Control.Monad
import System.IO
import Data.Char

class PipelineClass (p :: * -> * -> * -> *) where
	(=$=) :: p a b x -> p b c y -> p a c y

data Pipeline m i o r
	= HaveOutput o (Pipeline m i o r)
	| NeedInput (Maybe i -> Pipeline m i o r)
	| Done r
	| PipeM (m (Pipeline m i o r))

data Finished m i o r = Finished {
	finished :: m (),
	pipeline :: Pipeline m i o r }

fromDone :: Monad m => Pipeline m i o r -> m (Maybe r)
fromDone (Done x) = return $ Just x
fromDone (PipeM m) = do
	r <- m
	case r of
		Done x -> return $ Just x
		PipeM m' -> do
			r' <- m'
			fromDone r'
		_ -> return Nothing
fromDone _ = return Nothing

fromDoneF :: Monad m => Finished m i o r -> m (Maybe r)
fromDoneF (Finished f p) = do
	x <- fromDone p
	f
	return x

instance Functor m => PipelineClass (Pipeline m) where
	_ =$= Done y = Done y
	NeedInput f =$= p2 = NeedInput $ \i -> f i =$= p2
	p1 =$= HaveOutput o n = HaveOutput o (p1 =$= n)
	HaveOutput o n =$= NeedInput f = n =$= f (Just o)
	Done r =$= NeedInput f = Done r =$= f Nothing
	PipeM m =$= p2 = PipeM $ (=$= p2) <$> m
	p1 =$= PipeM m = PipeM $ (p1 =$=) <$> m

instance Functor m => Functor (Pipeline m i o) where
	f `fmap` HaveOutput o p = HaveOutput o $ f `fmap` p
	f `fmap` NeedInput n = NeedInput $ \i -> f `fmap` n i
	f `fmap` Done r = Done $ f r
	f `fmap` PipeM m = PipeM $ (f `fmap`) `fmap` m

instance Monad m => Monad (Pipeline m i o) where
	HaveOutput o p >>= f = HaveOutput o $ p >>= f
	NeedInput n >>= f = NeedInput $ n >=> f
	Done r >>= f = f r
	PipeM m >>= f = PipeM $ (>>= f) `liftM` m
	return = Done

liftP :: Monad m => m a -> Pipeline m i o a
liftP m = PipeM $ Done `liftM` m

instance (Monad m, Functor m) => PipelineClass (Finished m) where
	Finished f1 p1@(Done _) =$= Finished f2 p2 = Finished f2 $ do
		r <- p1 =$= p2
		liftP f1
		return r
	Finished f1 p1 =$= Finished f2 p2 = Finished (f1 >> f2) (p1 =$= p2)

pipe :: (a -> b) -> Pipeline m a b ()
pipe f = NeedInput $ \mi -> case mi of
	Just i -> HaveOutput (f i) (pipe f)
	_ -> Done ()

fromHandle :: Handle -> Pipeline IO () Char ()
fromHandle h = PipeM $ do
	c <- hGetChar h
	print c
	return $ case c of
		'q' -> Done ()
		_ -> HaveOutput c $ fromHandle h

toList :: Functor m => Pipeline m a () [a]
toList = NeedInput f
	where
	f Nothing = Done []
	f (Just x) = (x :) <$> toList

takeN :: Functor m => Int -> Pipeline m a () [a]
takeN 0 = Done []
takeN n = NeedInput f
	where
	f Nothing = Done []
	f (Just x) = (x :) <$> takeN (n - 1)

finishedHandle :: Handle -> IO () -> Finished IO () Char ()
finishedHandle h c = Finished c (fromHandle h)

liftF :: Monad m => Pipeline m i o r -> Finished m i o r
liftF = Finished $ return ()

readStdin :: Finished IO () Char ()
readStdin = finishedHandle stdin (putStrLn "finished")

test :: IO (Maybe String)
test = fromDoneF $ readStdin =$= liftF (pipe toUpper) =$= liftF (takeN 3)
