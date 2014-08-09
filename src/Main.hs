import Control.Applicative
import Options
import Control.Concurrent.ParallelIO.Global (parallel_, stopGlobalPool)

import After (afterPid, afterPartialCmdline)

data MainOptions = MainOptions
    { optQuiet :: Bool
    }

instance Options MainOptions where
    defineOptions = pure MainOptions
        <*> simpleOption "quiet" False
            "Whether to be quiet."

main :: IO ()
main = runCommand $ \opts args -> do
    if optQuiet opts
        then return ()
        else do
            parallel_ (map afterPartialCmdline args)
            stopGlobalPool

