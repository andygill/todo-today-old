module Main where

import Data.Char
import System.Environment
import Data.Map as Map
import System.Directory
import Data.List
import Data.Time.Calendar
import System.Cmd

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
         | Help
         | Edit
         | Status NSWD

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
                "need"     -> Status N
                "should"     -> Status S
                "want"     -> Status W
                "done"     -> Status D
                ('+':ns) | all isDigit ns -> Soon (read ns)

command :: Env -> TodoCmd -> Cmd -> IO ()
command env cmds (Soon n) = command env cmds' Add
  where cmds' = cmds { do_ = show n
                     }

command env cmds Help = do
        putStrLn $ unlines
                 [ "-- Commands (# == unimplemented)"
                 , "-- Adding Tasks"
                 , "% todo-today add   Remember the Milk    ;; undated task"
                 , "% todo-today today Remember the Milk    ;; task to be done today"
                 , "% todo-today +2    Remember the Milk    ;; task to be done in 2 days"
                 , "-- Showing Taks"
                 , "% todo-today                            ;; show todays task"
                 , "% todo-today  2 3                       ;; Show tasks #2 & 3"
                 , "# todo-today all                        ;; show *all* tasks"
                 , "-- Editing Tasks"
                 , "% todo-today edit 2 4                   ;; edit tasks #2 and #4"
                 , "# todo-today need   2 3                   ;; mark #2 & 3 as N"
                 , "# todo-today should 2 3                   ;; mark #2 & 3 as S"
                 , "# todo-today want   2 3                   ;; mark #2 & 3 as W"
                 , "# todo-today done   2 3                   ;; mark #2 & 3 as D"

                 ]
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
                system $ "emacs " ++
                        unwords [ env_todo env ++ "/" ++ show n ++ ".txt"
                                | n <- numbers cmds
                                ]
                return ()
command env cmds (Status s) = do
                updateTasks env cmds (\ t -> t { t_done = Just s })
                return ()

updateTasks env cmds f = do
        sequence_ [ writeDB (env_todo env) n (f t)
                  | n <- numbers cmds
                  , Just t <- [Map.lookup n (env_db env)]
                  ]
        return ()


numbers :: TodoCmd -> [Int]
numbers cmds = if length nums == 0
               then error "need at least one #number"
               else nums
  where
          nums =       [ read n :: Int
                      | n <- description cmds
                      , not (Prelude.null n)
                      , all isDigit n
                      ]

fullTitleLine = "    " ++ titleLine
fullTaskLine env i t = rjust 3 ' ' (show i) ++ " " ++ taskLine (env_today env) t

