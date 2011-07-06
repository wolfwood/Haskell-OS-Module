

import OS
import Text.Regex.Posix

main = do
  
  walk "/home/untwisted/projects/haskell" (\x -> True)
  