import Control.Applicative

class PipelineClass p where
	(=$=) :: p a b x -> p b c y -> p a c (x, y)

data Pipeline i o r
	= HaveOutput o (Pipeline i o r)
	| NeedInput (Maybe i -> Pipeline i o r)
	| Done r

fromDone :: Pipeline i o r -> Maybe r
fromDone (Done x) = Just x
fromDone _ = Nothing

instance PipelineClass Pipeline where
	Done x =$= Done y = Done (x, y)
	HaveOutput _ n =$= Done y = n =$= Done y
	NeedInput f =$= p2 = NeedInput $ \i -> f i =$= p2
	p1 =$= HaveOutput o' n' = HaveOutput o' (p1 =$= n')
	HaveOutput o n =$= NeedInput f = n =$= f (Just o)
	Done r =$= NeedInput f = Done r =$= f Nothing

instance Functor (Pipeline i o) where
	f `fmap` Done r = Done $ f r
	f `fmap` NeedInput n = NeedInput $ \i -> f `fmap` n i
	f `fmap` HaveOutput o n = HaveOutput o $ f `fmap` n

fromList :: [a] -> Pipeline () a ()
fromList = foldr HaveOutput (Done ())

toList :: Pipeline a () [a]
toList = NeedInput f
	where
	f Nothing = Done []
	f (Just x) = (x :) <$> toList
