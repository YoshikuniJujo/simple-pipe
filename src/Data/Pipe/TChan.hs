{-# LANGUAGE FlexibleContexts, PackageImports #-}

module Data.Pipe.TChan (
	fromTChan, fromTChans, toTChan,
	) where

import Control.Applicative
import "monads-tf" Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Base
import Control.Concurrent.STM
import Data.Pipe

fromTChan :: MonadBaseControl IO m => TChan a -> Pipe () a m ()
fromTChan c = lift (liftBase . atomically $ readTChan c) >>= yield >> fromTChan c

fromTChans :: MonadBaseControl IO m => [TChan a] -> Pipe () a m ()
fromTChans cs = (>> fromTChans cs) . (yield =<<) . lift . liftBase . atomically $ do
	readTChans cs >>= maybe retry return

readTChans :: [TChan a] -> STM (Maybe a)
readTChans [] = return Nothing
readTChans (c : cs) = do
	e <- isEmptyTChan c
	if e then readTChans cs else Just <$> readTChan c

toTChan :: MonadBaseControl IO m => TChan a -> Pipe a () m ()
toTChan c = await >>= maybe (return ())
	((>> toTChan c) . lift . liftBase . atomically . writeTChan c)
