module After where

import System.INotify
import Control.Concurrent

justShowAndUnlock :: MVar String -> Event -> IO()
justShowAndUnlock mvar event = do
    putStrLn (show event)
    putMVar mvar "Yep, this is happening"

afterPid :: String -> IO ()
afterPid pid = do
    inotifyHandle <- initINotify
    unockedAfter <- newEmptyMVar
    watchHandle <- addWatch inotifyHandle [DeleteSelf, MoveSelf, OnlyDir] ("/proc/" ++ pid) (justShowAndUnlock unockedAfter)
    seen <- takeMVar unockedAfter
    putStrLn seen

