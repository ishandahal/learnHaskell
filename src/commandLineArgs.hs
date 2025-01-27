import Data.List (delete)
import System.Directory (removeFile, renameFile)
import System.Directory.Internal.Prelude (getArgs, hClose, hPutStr)
import System.IO (openFile, openTempFile)

dispatch :: [(String, [String] -> IO ())]
dispatch =
  [ ("add", add),
    ("display", display),
    ("remove", remove)
  ]

errorExit :: IO ()
errorExit = return ()

main = do
  (command : args) <- getArgs
  -- let Just action = lookup command dispatch
  case lookup command dispatch of
    Just action -> action args
    Nothing -> do
      putStrLn "Command not found. Please try again with a valid command."
      errorExit

add :: [String] -> IO ()
add [fileName, task] = appendFile fileName (task ++ ['\n'])

display :: [String] -> IO ()
display [fileName] = do
  contents <- readFile fileName
  let allTasks = zipWith (\line n -> show n ++ " - " ++ line) (lines contents) [0 ..]
  mapM_ putStrLn allTasks

remove :: [String] -> IO ()
remove [fileName, numberString] = do
  (tempFile, tempHandle) <- openTempFile "." "temp"
  contents <- readFile fileName
  let allTasks = lines contents
      number = read numberString
      updatedTasks = delete (allTasks !! number) allTasks
  hPutStr tempHandle (unlines updatedTasks)
  hClose tempHandle
  removeFile fileName
  renameFile tempFile fileName
