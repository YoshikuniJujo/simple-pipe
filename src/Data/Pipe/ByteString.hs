{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, PackageImports #-}

module Data.Pipe.ByteString (fromHandleLn, toHandleLn, fromFileLn, toFileLn) where

import Control.Applicative
import "monads-tf" Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Exception (catch)
import Data.Pipe
import System.IO
import GHC.IO.Exception

import qualified Data.ByteString.Char8 as BSC

fromHandleLn :: (PipeClass p, Monad m, MonadIO (p i BSC.ByteString m)) =>
	Handle -> p i BSC.ByteString m ()
fromHandleLn h = do
	ml <- liftIO $ (Just <$> BSC.hGetLine h) `catch` \(e :: IOException) ->
		case ioe_type e of
			EOF -> return Nothing
			_ -> ioException e
	maybe (return ()) ((>> fromHandleLn h) . yield) ml

toHandleLn :: (PipeClass p, Monad m, MonadIO (p BSC.ByteString o m)) =>
	Handle -> p BSC.ByteString o m ()
toHandleLn h =
	await >>= maybe (return ()) ((>> toHandleLn h) . liftIO . BSC.hPutStrLn h)

fromFileLn :: (PipeClass p, MonadIO m, MonadBaseControl IO m,
	MonadTrans (p i BSC.ByteString), MonadIO (p i BSC.ByteString m)) =>
	FilePath -> p i BSC.ByteString m ()
fromFileLn fp =
	bracket (liftIO $ openFile fp ReadMode) (liftIO . hClose) fromHandleLn

toFileLn :: (PipeClass p, MonadIO m, MonadBaseControl IO m,
	MonadTrans (p BSC.ByteString o), MonadIO (p BSC.ByteString o m)) =>
	FilePath -> p BSC.ByteString o m ()
toFileLn fp = bracket (liftIO $ openFile fp WriteMode) (liftIO . hClose) toHandleLn
