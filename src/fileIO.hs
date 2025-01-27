import Data.Char (toUpper)
import Data.List (delete)
import System.Directory (removeFile, renameFile)
import System.IO

-- main = do
--   handle <- openFile "haiku.txt" ReadMode
--   contents <- hGetContents handle
--   putStr contents
--   hClose handle

-- main = do
--   withFile'
--     "haiku.txt"
--     ReadMode
--     ( \handle -> do
--         hSetBuffering handle $ BlockBuffering (Just 2048)
--         contents <- hGetContents handle
--         putStr contents
--     )

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' filePath mode f = do
  handle <- openFile filePath mode
  result <- f handle
  hClose handle
  return result

-- main = do
--   contents <- readFile "haiku.txt"
--   putStr contents

-- main = do
--   contents <- readFile "haiku.txt"
-- writeFile "haikuUpper.txt" (map toUpper contents)

-- main = do
--   line <- getLine
--   appendFile "todo.txt" (line ++ "\n")

main = do
  handle <- openFile "todo.txt" ReadMode
  (tempFile, tempFileHandle) <- openTempFile "." "temp"
  putStrLn $ "Temp filename: " ++ tempFile
  contents <- hGetContents handle
  let tasks = lines contents
      indexedTasks = zipWith (\task idx -> show idx ++ " " ++ task) tasks [0 ..]
  putStrLn "Here are the tasks in the todo list."
  mapM_ putStrLn indexedTasks
  putStrLn "Which task would you like to remove?"
  number_string <- getLine
  let number = read number_string
      updatedList = delete (tasks !! number) tasks
  hPutStr tempFileHandle $ unlines updatedList
  hClose handle
  hClose tempFileHandle
  removeFile "todo.txt"
  renameFile tempFile "todo.txt"
