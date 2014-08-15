import Data.Pipe
import Data.Pipe.ByteString
import System.IO

main :: IO ()
main = do
	_ <- runPipe $ fromFileLn "test/sample.txt" =$= toHandleLn stdout
	return ()
