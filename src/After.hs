module After where

import Control.Concurrent
import System.Directory
import Data.List (isInfixOf, delete)
import Process (psCmdLine, psListing)
import System.Posix.Process (getProcessID)
import Control.Monad (filterM)

halfASecondInMicroseconds = 500000

waitForPid :: String -> IO ()
waitForPid pid = do
    fileExists <- doesDirectoryExist ("/proc/" ++ pid)
    if fileExists
    then do
        threadDelay halfASecondInMicroseconds
        waitForPid pid
    else return ()


-- afterPid will block until the given pid has exited
afterPid :: String -> IO ()
afterPid pid = do
    cmdLine <- psCmdLine pid
    putStrLn $ "Waiting for " ++ pid ++ ": " ++ cmdLine
    waitForPid pid


pidHasPartialCommand :: String -> String -> IO Bool
pidHasPartialCommand needle pid = do
    cmd <- psCmdLine pid
    return $ isInfixOf needle cmd

pidsWithPartialCommand :: String -> IO [String]
pidsWithPartialCommand cmdLine = do
    listing <- psListing
    filterM (pidHasPartialCommand cmdLine) listing

afterPartialCmdline :: String -> IO()
afterPartialCmdline cmdLine = do
    pids <- pidsWithPartialCommand cmdLine
    ownPid <- getProcessID
    let otherPids = delete (show ownPid) pids
    mapM_ afterPid otherPids
