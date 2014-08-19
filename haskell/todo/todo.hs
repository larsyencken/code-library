--
--  todo.hs
--
--  A simple todo list manager.
--

import System.Environment
import System.Directory
import System.IO

todoFile = "todo.txt"

main = do
    args <- getArgs
    let safeArgs = if (length args == 0) then ["help"] else args
    dispatch safeArgs

dispatch :: [String] -> IO ()
dispatch ("add":[task]) = add task
dispatch ["list"] = list
dispatch ("delete":[task]) = delete task
dispatch _ = usage

usage :: IO ()
usage = mapM_ putStrLn [
    "Usage:",
    "",
    "  todo add <task>          Add a task to the list",
    "  todo list                List all tasks",
    "  todo delete <taskId>     Remove a numbered task from the list"
    ]

add :: String -> IO ()
add task = do
    appendFile todoFile (task ++ "\n")

list :: IO ()
list = do
    contents <- readFile todoFile
    let tasks = lines contents
    mapM_ putStrLn $ zipWith (\n t -> show n ++ ": " ++ t) [1..] tasks

delete :: String -> IO ()
delete taskId = do
    contents <- readFile todoFile
    let todoTasks = zip [1..] $ lines contents
    let todoTasks' = filter (\t -> (fst t /= (read taskId :: Int))) todoTasks
    let contents' = unlines $ map snd todoTasks'
    saveFile contents' todoFile

saveFile :: String -> String -> IO ()
saveFile contents filename = do
    (tempName, tempHandle) <- openTempFile "." "temp"
    hPutStr tempHandle contents
    hClose tempHandle
    removeFile filename
    renameFile tempName filename
