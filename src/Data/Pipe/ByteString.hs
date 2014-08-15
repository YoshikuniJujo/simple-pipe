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

fromHandleLn :: MonadIO m => Handle -> Pipe () BSC.ByteString m ()
fromHandleLn h = do
	ml <- liftIO $ (Just <$> BSC.hGetLine h) `catch` \(e :: IOException) ->
		case ioe_type e of
			EOF -> return Nothing
			_ -> ioException e
	maybe (return ()) ((>> fromHandleLn h) . yield) ml

toHandleLn :: MonadIO m => Handle -> Pipe BSC.ByteString () m ()
toHandleLn h =
	await >>= maybe (return ()) ((>> toHandleLn h) . liftIO . BSC.hPutStrLn h)

fromFileLn :: (MonadIO m, MonadBaseControl IO m) =>
	FilePath -> Pipe () BSC.ByteString m ()
fromFileLn fp =
	bracket (liftIO $ openFile fp ReadMode) (liftIO . hClose) fromHandleLn

toFileLn :: (MonadIO m, MonadBaseControl IO m) =>
	FilePath -> Pipe BSC.ByteString () m ()
toFileLn fp = bracket (liftIO $ openFile fp WriteMode) (liftIO . hClose) toHandleLn
