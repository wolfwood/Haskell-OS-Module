-- OS module to mimic many of the features of the Python OS module.
-- Will provide convenience functions for many typical system tasks.

module OS where

import System.Directory
import System.FilePath
import Text.Printf
import Monad

-- |walk will traverse a directory tree rooted at d and execute the
-- filter function f at each node. walk returns a list of absolute
-- FilePaths that passed the filter function. walk will filter . and
-- .. from the traversed directories.

walk :: FilePath -> (FilePath -> Bool) -> IO [FilePath]
walk d f = do

  -- Set the current directory
  setCurrentDirectory d

  -- Now get current directory
  files <- getDirectoryContents d

  -- Filter out . and ..
  let files' = filter (\x -> x /= "." && x /= "..") files

  -- Now run the user defined filter
  let files'' = filter f files'

  -- Add the full path to the file names
  let acc = map (d </>) files''

  -- Get just the sub-directories and start traversing them as well
  subd <- filterM (doesDirectoryExist) files'

  -- Put the full path on the directories as well
  let subd' = map (d </>) subd

  -- Perform the actual walk
  foo <- mapM (`walk` f) subd'

  let acc' = concat foo

  return (acc' ++ acc)

walkM :: FilePath -> (FilePath -> IO Bool) -> IO [FilePath]
walkM d f = do

  -- Set the current directory
  setCurrentDirectory d

  -- Now get current directory
  files <- getDirectoryContents d

  -- Filter out . and ..
  let files' = filter (\x -> x /= "." && x /= "..") files

  -- Now run the user defined filter
  files'' <- filterM f files'

  -- Add the full path to the file names
  let acc = map (d </>) files''

  -- Get just the sub-directories and start traversing them as well
  subd <- filterM (doesDirectoryExist) files'

  -- Put the full path on the directories as well
  let subd' = map (d </>) subd

  -- Perform the actual walk
  foo <- mapM (`walkM` f) subd'

  let acc' = concat foo

  return (acc' ++ acc)


