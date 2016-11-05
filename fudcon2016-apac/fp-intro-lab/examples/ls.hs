import System.Directory

main = do
  files <- getDirectoryContents "."
  let files' = filter (`notElem` [".", ".."]) files 
  mapM_ putStrLn files'
