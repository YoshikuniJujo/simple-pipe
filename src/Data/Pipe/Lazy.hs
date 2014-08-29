{-# LANGUAGE FlexibleContexts #-}

module Data.Pipe.Lazy (PipeLazy(..)) where

import Control.Monad
import Control.Monad.Trans.Control
import qualified System.IO.Unsafe as U

import Data.Pipe.Core

class PipeClass pl => PipeLazy pl where
	-- | (io >>= toLazy . fromList) == io
	toLazy :: MonadBaseControl IO m => pl i o m r -> m [o]

instance PipeLazy Pipe where toLazy = toLazyP

toLazyP :: MonadBaseControl IO m => Pipe i o m r -> m [o]
toLazyP (Ready f o p) = (o :) `liftM` unsafeInterleaveIO (toLazyP p)
toLazyP (Need f p) = toLazyP $ p Nothing
toLazyP (Done f r) = return []
toLazyP (Make f p) = toLazyP =<< p

unsafeInterleaveIO :: MonadBaseControl IO m => m a -> m a
unsafeInterleaveIO m = control $ \runInIO -> U.unsafeInterleaveIO (runInIO m)
