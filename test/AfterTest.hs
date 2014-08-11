
import Test.Framework.Runners.Console (defaultMain)
import Test.Framework.Providers.HUnit (testCase)

main :: IO ()
main = defaultMain [
        testCase "neverSelf" testSelf
    ]

--------------------------------
-- Unit tests
--------------------------------
testSelf = putStrLn "Yeah"
