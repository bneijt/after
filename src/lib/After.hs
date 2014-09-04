module After where
-- This module defines blocking `-> IO()` functions
-- which can be used to monitor for process termination.


import Control.Concurrent
import System.Directory
import Data.List (isInfixOf)
import Process (psCmdLine, psListing)
import System.Posix.Process (getProcessID)
import Control.Monad (filterM, unless, when)
import Control.Exception -- (try, SomeException)

halfASecondInMicroseconds :: Int
halfASecondInMicroseconds = 500000

waitForPid :: String -> IO ()
waitForPid pid = do
    fileExists <- doesDirectoryExist ("/proc/" ++ pid)
    when fileExists $ do
        threadDelay halfASecondInMicroseconds
        waitForPid pid



-- afterPid will block until the given pid has exited
afterPid :: String -> IO ()
afterPid pid = do
    ownPid <- getProcessID
    unless (pid == show ownPid) $ do
        cmdLine <- psCmdLine pid
        putStrLn $ "Waiting for " ++ pid ++ ": " ++ cmdLine
        waitForPid pid

pidHasPartialCommand :: String -> String -> IO Bool
pidHasPartialCommand needle pid = do
    cmd <- try (psCmdLine pid) :: IO (Either IOException String)
    case cmd of
        Right x -> return $ isInfixOf needle x
        Left _ -> return False

pidsWithPartialCommand :: String -> IO [String]
pidsWithPartialCommand cmdLine = do
    listing <- psListing
    filterM (pidHasPartialCommand cmdLine) listing

afterPartialCmdline :: String -> IO()
afterPartialCmdline cmdLine = do
    pids <- pidsWithPartialCommand cmdLine
    mapM_ afterPid pids
