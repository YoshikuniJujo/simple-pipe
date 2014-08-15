{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, PackageImports #-}

module Data.Pipe.IO (fromHandle, toHandle, fromFile, toFile) where

import Control.Applicative
import "monads-tf" Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Exception (catch)
import Data.Pipe
import System.IO
import GHC.IO.Exception

fromHandle :: MonadIO m => Handle -> Pipe () Char m ()
fromHandle h = do
	mc <- liftIO $ (Just <$> hGetChar h) `catch` \(e :: IOException) ->
		case ioe_type e of
			EOF -> return Nothing
			_ -> ioException e
	maybe (return ()) ((>> fromHandle h) . yield) mc

toHandle :: MonadIO m => Handle -> Pipe Char () m ()
toHandle h = await >>= maybe (return ()) ((>> toHandle h) . liftIO . hPutChar h)

fromFile :: (MonadIO m, MonadBaseControl IO m) => FilePath -> Pipe () Char m ()
fromFile fp = bracket (liftIO $ openFile fp ReadMode) (liftIO . hClose) fromHandle

toFile :: (MonadIO m, MonadBaseControl IO m) => FilePath -> Pipe Char () m ()
toFile fp = bracket (liftIO $ openFile fp WriteMode) (liftIO . hClose) toHandle
