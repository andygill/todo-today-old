module Main where

import Data.Char
import System.Environment
import Data.Map as Map
import System.Directory
import Data.List
import Data.Time.Calendar

import Task
import Command
import DB
import Paths_todo_today

resolveTodoList xs
  | "/Users/andy/Library/Haskell" `isPrefixOf` xs = "/Users/andy/.todo"
  | "/Users/andy/git"             `isPrefixOf` xs = "todo"
  | otherwise                                     = error $ "can not resolve location of todos from " ++ xs

main = do
        dir  <- getExecutablePath
        let todo = resolveTodoList dir
        cmds <- parseCmds
        today <- todayIs
        db <- loadDB todo
        let (cmd, cmds1) = parseCmd cmds
        command (Env todo db today) cmds1 cmd

data Env = Env
        { env_todo  :: String   -- directory to find the todos
        , env_db    :: DB       -- The database
        , env_today :: Day      -- what is the date today
        }

data Cmd = Add
         | Show
         | Soon Int     -- in future, #n, n is days to go
         | Mod Int      -- mod a specific record number
         | Help
         | Edit


parseCmd :: TodoCmd -> (Cmd,TodoCmd)
parseCmd todoCmd@(TodoCmd { description = [] }) = (Show, todoCmd)
parseCmd todoCmd@(TodoCmd { description = mode : args }) = (cmd, todoCmd { description = args })
  where cmd = case mode of
                "add"  -> Add
                "show" -> Show
                "edit" -> Edit
                "help" -> Help
                "today" -> Soon 0
                "tomorrow" -> Soon 1
                ('+':ns) | all isDigit ns -> Soon (read ns)
                ('#':ns) | all isDigit ns -> Mod (read ns)

command :: Env -> TodoCmd -> Cmd -> IO ()
command env cmds (Soon n) = command env cmds' Add
  where cmds' = cmds { do_ = show n
                     }

command env cmds Help = do
        putStrLn $ "modes: add | dump | today | +n | #n | edit"

command env cmds Show = do
  let db = env_db env
  let xs = [ fullTaskLine env i t
            | (i,t) <- Map.toList db
            , Prelude.null (description cmds) ||
                show i `elem` (description cmds) ||
                showNSWD (t_done t) `elem` (description cmds)
           ]
  if Prelude.null xs then return () else do
          putStrLn $ fullTitleLine
          putStr $ unlines xs

command env cmds Add = do
   let today = env_today env
   td_by <- case readTaskDay today (by cmds) of
           Just td -> return td
           Nothing -> error $ "bad format for date : " ++ show (by cmds)

   td_do <- case readTaskDay today (do_ cmds) of
           Just td -> return td
           Nothing -> error $ "bad format for date : " ++ show (do_ cmds)

   let task = Task
           { t_done = done cmds
           , t_dur = case span isDigit (duration cmds) of
                      ([],[]) -> Nothing
                      (n,"")  -> return $ read n
                      (n,"m") -> return $ read n
                      (n,"h") -> return $ read n * 60
                      _ -> error $ "duration error " ++ show (duration cmds)
           , t_task = mkLine $ unwords $ description cmds
           , t_by = td_by
           , t_do = td_do
           , t_pri = 0
           }
   let db = env_db env
   let new_number = newUniqDB db
   writeDB (env_todo env) new_number task
   putStrLn $ fullTitleLine
   putStrLn $ fullTaskLine env new_number task
command env cmds Edit = do
        putStrLn $ "EDIT!"

fullTitleLine = "    " ++ titleLine
fullTaskLine env i t = rjust 3 ' ' (show i) ++ " " ++ taskLine (env_today env) t

