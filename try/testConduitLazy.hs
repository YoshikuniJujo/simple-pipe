{-# LANGUAGE PackageImports #-}

import "monads-tf" Control.Monad.Trans
import Control.Applicative
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Lazy
import Data.Time

times :: Int -> ConduitM i UTCTime IO ()
times 0 = return ()
times n = lift getCurrentTime >>= yield >> times (n - 1)
