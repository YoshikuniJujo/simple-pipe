{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, PackageImports #-}

module Data.Pipe.IO (fromHandle, toHandle, fromFile, toFile) where

import Control.Applicative
import "monads-tf" Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Exception (catch)
import Data.Pipe
import System.IO
import GHC.IO.Exception

fromHandle :: (PipeClass p, Monad m, MonadIO (p i Char m)) =>
	Handle -> p i Char m ()
fromHandle h = do
	mc <- liftIO $ (Just <$> hGetChar h) `catch` \(e :: IOException) ->
		case ioe_type e of
			EOF -> return Nothing
			_ -> ioException e
	maybe (return ()) ((>> fromHandle h) . yield) mc

toHandle :: (PipeClass p, Monad m, MonadIO (p Char o m)) =>
	Handle -> p Char o m ()
toHandle h = await >>= maybe (return ()) ((>> toHandle h) . liftIO . hPutChar h)

fromFile :: (PipeClass p, MonadIO m, MonadBaseControl IO m,
	MonadTrans (p i Char), MonadIO (p i Char m)) => FilePath -> p i Char m ()
fromFile fp = bracket (liftIO $ openFile fp ReadMode) (liftIO . hClose) fromHandle

toFile :: (PipeClass p, MonadIO m, MonadBaseControl IO m,
	MonadTrans (p Char o), MonadIO (p Char o m)) => FilePath -> p Char o m ()
toFile fp = bracket (liftIO $ openFile fp WriteMode) (liftIO . hClose) toHandle
