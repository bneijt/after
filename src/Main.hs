import Control.Applicative
import Options

import After (afterPid)

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
        else afterPid (head args)

