
import Control.Applicative
import System.IO.Unsafe

nums :: Int -> IO [Int]
nums n = unsafeInterleaveIO $ print n >> (n :) <$> nums (n + 1)
