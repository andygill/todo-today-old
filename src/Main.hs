module Main where

import Data.Char
import System.Environment
import Data.Map as Map
import System.Directory
import Data.List as L
import Data.Time.Calendar
import System.Cmd
import Data.Monoid

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

data Cmd = Add          Selector
         | Show         Selector
         | Soon Int     Selector -- in future, #n, n is days to go
         | Help
         | Edit         Selector
         | Status NSWD  Selector
         | Gc
  deriving Show

data Selector = Selector [Int]
              | None
  deriving Show

select :: (Int, t0) -> Selector -> Bool
select (i,t) (Selector nums) = i `elem` nums
select _     None      = True

parseCmd :: TodoCmd -> (Cmd,TodoCmd)
parseCmd todoCmd@(TodoCmd { description = [] }) = (Show None, todoCmd)
parseCmd todoCmd@(TodoCmd { description = mode : args })
        | all isDigit mode = (Show (numbers (mode : args)), todoCmd)
        | otherwise = (cmd, todoCmd { description = args })
  where cmd = case mode of
                "add"  -> Add (numbers args)
                "show" -> Show (numbers args)
                "edit" -> Edit (numbers args)
                "help" -> Help
                "today" -> Soon 0 (numbers args)
                "tomorrow" -> Soon 1 (numbers args)
                "need"     -> Status N (numbers args)
                "should"   -> Status S (numbers args)
                "want"     -> Status W (numbers args)
                "done"     -> Status D (numbers args)
                "gc"       -> Gc
                ('+':ns) | all isDigit ns -> Soon (read ns) (numbers args)

command :: Env -> TodoCmd -> Cmd -> IO ()
command env cmds (Soon n sel) = command env cmds' (Add sel)
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
                 , "# todo-today                            ;; show todays task"
                 , "# todo-today  2 3                       ;; Show tasks #2 & 3"
                 , "% todo-today show                        ;; show *all* tasks"
                 , "-- Editing Tasks"
                 , "% todo-today edit 2 4                   ;; edit tasks #2 and #4"
                 , "% todo-today need   2 3                   ;; mark #2 & 3 as N"
                 , "% todo-today should 2 3                   ;; mark #2 & 3 as S"
                 , "% todo-today want   2 3                   ;; mark #2 & 3 as W"
                 , "% todo-today done   2 3                   ;; mark #2 & 3 as D"
                 , "% todo-today gc                          ;; delete done task"
                 ]
command env cmds (Show sel) = do
  let db = env_db env
  let xs = [ (t, fullTaskLine env i t)
            | (i,t) <- Map.toList db
            , select (i,t) sel
--            Prelude.null nums || i `elem` nums
-- TODO                || showNSWD (t_done t) `elem` (description cmds)
           ]
  let xs1 = sortBy (\ (t1,_) (t2,_) -> t_do t1 `compare` t_do t2) xs

  let xs2 = L.map snd xs1

  if Prelude.null xs2 then return () else do
          putStrLn $ fullTitleLine
          putStr $ unlines $ xs2

command env cmds (Add None) = do
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
command env cmds cmd@(Add (Selector sel)) = do
   let today = env_today env
   td_by <- case readTaskDay today (by cmds) of
           Just td -> return td
           Nothing -> error $ "bad format for date : " ++ show (by cmds)

   td_do <- case readTaskDay today (do_ cmds) of
           Just td -> return td
           Nothing -> error $ "bad format for date : " ++ show (do_ cmds)


   -- Now, change what need changes
   print (cmds,cmd,td_by,td_do)
   updateTasks env sel $ \ task -> task
        { t_by = td_by <> t_by task
        , t_do = td_do <> t_do task
        }
{-
   Task {
        t_by = case t_by task of

                Ttd_do
-}
{-
      Task { t_done	:: !(Maybe NSWD)        -- ^ Have you done this, and do you want to
     , t_dur    :: !(Maybe Int)	        -- ^ How long, in minutes, to complete
     , t_by	:: !TaskDay	        -- ^ When must this be done by
     , t_do	:: !TaskDay	        -- ^ When do I plan to to/review this.
     , t_pri    :: !Rational            -- ^ Priority when sorting
     , t_task 	:: !Line              	-- ^ Description of what needs done
     }
-}
command env cmds (Edit sel) = do
                system $ "emacs " ++
                        unwords [ env_todo env ++ "/" ++ show n ++ ".txt"
                                | n <- case sel of
                                         None -> error "can not edit None"
                                         Selector nums -> nums
                                ]
                return ()
command env cmds (Status s None) = error "can not change status of None"
command env cmds (Status s (Selector sel)) = do
                updateTasks env sel (\ t -> t { t_done = Just s })
                return ()

command env cmds Gc = do
        let to_gc = [ i
                    | (i,t) <- Map.toList (env_db env)
                    , t_done t == Just D
                    ]
        sequence_ [ renameFile (env_todo env ++ "/" ++ show n ++ ".txt")
                               (env_todo env ++ "/" ++ show n ++ ".done")
                  | n <- to_gc
                  ]
        putStrLn $ "Deleted Done Tasks: " ++ show to_gc

updateTasks :: Env -> [Int] -> (Task -> Task) -> IO ()
updateTasks env nums f = do
        sequence_ [ writeDB (env_todo env) n t'
                  | n <- nums
                  , Just t <- [Map.lookup n (env_db env)]
                  , let t' = f t
                  , t' /= t     -- there is something that changed
                  ]
        return ()


numbers :: [String] -> Selector
numbers cmds | Prelude.null nums = None
             | otherwise         = Selector nums
   where
        nums = [ read n :: Int
               | n <- cmds
               , not (Prelude.null n)
               , all isDigit n
               ]

fullTitleLine = "    " ++ titleLine
fullTaskLine env i t = rjust 3 ' ' (show i) ++ " " ++ taskLine (env_today env) t

