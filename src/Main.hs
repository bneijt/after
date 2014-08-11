import Control.Applicative
import Options
import Control.Concurrent.ParallelIO.Global (parallel_, stopGlobalPool)

import Paths_after (version)
import Data.Version (showVersion)

import After (afterPartialCmdline)

data MainOptions = MainOptions
    { optVersion :: Bool
    }

instance Options MainOptions where
    defineOptions = pure MainOptions
        <*> simpleOption "version" False
            "Show the program version"

main :: IO ()
main = runCommand $ \opts args -> do
    if optVersion opts
        then putStrLn ("after " ++ showVersion version)
        else do
            parallel_ (map afterPartialCmdline args)
            stopGlobalPool

