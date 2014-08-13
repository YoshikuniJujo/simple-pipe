{-# LANGUAGE FlexibleContexts, PackageImports #-}

module Data.Pipe.IO (fromHandle, toHandle, fromFile, toFile) where

import "monads-tf" Control.Monad.Trans
import Control.Monad.Trans.Control
import Data.Pipe
import System.IO

fromHandle :: MonadIO m => Handle -> Pipe () Char m ()
fromHandle h = liftIO (hGetChar h) >>= (>> fromHandle h) . yield

toHandle :: MonadIO m => Handle -> Pipe Char () m ()
toHandle h = await >>= maybe (return ()) ((>> toHandle h) . liftIO . hPutChar h)

fromFile :: (MonadIO m, MonadBaseControl IO m) => FilePath -> Pipe () Char m ()
fromFile fp = bracket (liftIO $ openFile fp ReadMode) (liftIO . hClose) fromHandle

toFile :: (MonadIO m, MonadBaseControl IO m) => FilePath -> Pipe Char () m ()
toFile fp = bracket (liftIO $ openFile fp WriteMode) (liftIO . hClose) toHandle
