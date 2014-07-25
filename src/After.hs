module After where

import Control.Concurrent
import System.Directory

halfASecondInMicroseconds = 500000

afterPid :: String -> IO ()
afterPid pid = do
    fileExists <- doesDirectoryExist ("/proc/" ++ pid)
    if fileExists
    then do
        threadDelay halfASecondInMicroseconds
        afterPid pid
    else return ()



