module Process where
import System.Directory (getDirectoryContents)
import System.FilePath ((</>))
import Data.Char (isDigit)

-- Filter /proc directories that contain numbers
psListing :: IO [FilePath]
psListing = do
    listing <- getDirectoryContents "/proc"
    return $ filter (all isDigit :: FilePath -> Bool) listing

-- Load the /proc/<pid>/cmdline contents
-- concats arguments using ' ' (argument boundry information is lost!)
psCmdLine :: String -> IO String
psCmdLine pid = do
    line <- readFile ("/proc" </> pid </> "cmdline")
    return $ map (\x -> if x == '\0' then ' ' else x) line
