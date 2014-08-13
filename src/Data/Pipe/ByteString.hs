{-# LANGUAGE FlexibleContexts, PackageImports #-}

module Data.Pipe.ByteString (fromHandleLn, toHandleLn, fromFileLn, toFileLn) where

import "monads-tf" Control.Monad.Trans
import Control.Monad.Trans.Control
import Data.Pipe
import System.IO

import qualified Data.ByteString.Char8 as BSC

fromHandleLn :: MonadIO m => Handle -> Pipe () BSC.ByteString m ()
fromHandleLn h = liftIO (BSC.hGetLine h) >>= (>> fromHandleLn h) . yield

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
