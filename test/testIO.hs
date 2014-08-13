{-# LANGUAGE PackageImports #-}

import "monads-tf" Control.Monad.Trans
import Data.Pipe
import Data.Pipe.List

getP :: Pipe () Char IO ()
getP = do
	c <- lift getChar
	lift $ print c
	yield c
	getP

putP :: Pipe Char () IO ()
putP = do
	mc <- await
	case mc of
		Just c -> lift (print c) >> putP
		_ -> return ()
