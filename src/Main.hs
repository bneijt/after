import Control.Applicative
import Options
import Control.Concurrent.ParallelIO.Global (parallel_, stopGlobalPool)

import Paths_after (version)
import Data.Version (showVersion)

import After (afterPartialCmdline, afterPid)

data MainOptions = MainOptions
    { optVersion :: Bool,
      optPid :: [Int]
    }

instance Options MainOptions where
    defineOptions = pure MainOptions
        <*> simpleOption "version" False
            "Show the program version"
        <*> defineOption (optionType_list ',' optionType_int) (\o -> o
            {
                optionLongFlags = ["pid"],
                optionShortFlags = ['p']
            })

main :: IO ()
main = runCommand $ \opts args -> do
    if optVersion opts
        then putStrLn ("after " ++ showVersion version)
        else do
            parallel_ $ (map afterPartialCmdline args) ++ map (afterPid . show) (optPid opts)
            stopGlobalPool

