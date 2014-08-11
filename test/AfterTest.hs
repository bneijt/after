
import Test.Framework.Runners.Console (defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import System.Posix.Process (getProcessID)
import After (afterPid)

main :: IO ()
main = defaultMain [
        testCase "neverSelf" testSelf
    ]

--------------------------------
-- Unit tests
--------------------------------

-- This test should simply continue as afterPid self should immediately return
testSelf = do
    self <- getProcessID
    afterPid (show self)

