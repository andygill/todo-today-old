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

todo = "todo"

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
        command (Env todo db today) cmds

data Env = Env
        { env_todo  :: String   -- directory to find the todos
        , env_db    :: DB       -- The database
        , env_today :: Day      -- what is the date today
        }

command :: Env -> TodoCmd -> IO ()
command env (cmd@Dump{}) = do
  let db = env_db env
  putStrLn $ fullTitleLine
  sequence_ [ putStrLn $ fullTaskLine env i t
            | (i,t) <- Map.toList db
            ]
command env (cmd@Add{}) = do
   let today = env_today env
   td_by <- case readTaskDay today (by cmd) of
           Just td -> return td
           Nothing -> error $ "bad format for date : " ++ show (by cmd)

   td_do <- case readTaskDay today (do_ cmd) of
           Just td -> return td
           Nothing -> error $ "bad format for date : " ++ show (do_ cmd)

   let task = Task
           { t_done = done cmd
           , t_dur = case span isDigit (duration cmd) of
                      ([],[]) -> Nothing
                      (n,"")  -> return $ read n
                      (n,"m") -> return $ read n
                      (n,"h") -> return $ read n * 60
                      _ -> error $ "duration error " ++ show (duration cmd)
           , t_task = mkLine $ unwords $ description cmd
           , t_by = td_by
           , t_do = td_do
           , t_pri = 0
           }
   db <- loadDB todo
   let new_number = newUniqDB db
   writeDB todo new_number task
   putStrLn $ fullTitleLine
   putStrLn $ fullTaskLine env new_number task

fullTitleLine = "    " ++ titleLine
fullTaskLine env i t = rjust 3 ' ' (show i) ++ " " ++ taskLine (env_today env) t ++ "!"

