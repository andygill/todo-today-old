-- Rework of the TODO framework

import Control.Monad
import Data.Char
import qualified Data.Map as M
import Data.Map (Map)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import System.Environment
import System.Exit
import System.Directory
import System.Locale
import System.Posix.Directory
import System.Process
--------------------------------------------------------------------

rjust :: Int -> Char -> String -> String
rjust n c xs = take n (take (n - length xs) (repeat c) ++ xs)

ljust :: Int -> Char -> String -> String
ljust n c xs = take n (xs ++ take (n - length xs) (repeat c))

showLongDay :: Day -> Day -> String
showLongDay today day = prefix ++ showDay day
  where diff = diffDays day today
        (y,m,d) = toGregorian day
        the_day = formatTime defaultTimeLocale "%a" day
        suff 1 = "st"
        suff 2 = "nd"
        suff n = "th"
        prefix
           | diff == 0 = "Today, "
           | diff == 1 = "Tomorrow, "
           | diff > 0 && diff < 7 = the_day ++ " " ++ show d ++ suff d ++ ", "
           | otherwise = ""

showDay :: Day -> String
showDay day = rjust 2 '0' (show m) ++ "/" ++
              rjust 2 '0' (show d) ++ "/" ++
              rjust 2 '0' (show (y `mod` 100))
  where (y,m,d) = toGregorian day

y2k y = 2000 + y

readsDay :: String -> [(Day,String)]
readsDay str0 =  [ (fromGregorian (y2k year) month day,str5)
                | (month,str1) <- reads str0
                , ("/",  str2) <- lex   str1
                , (day,  str3) <- reads str2
                , ("/",  str4) <- lex   str3
                , (year, str5) <- reads str4
                ]


--readDay :: Day -> String -> Maybe (Day,String)

--------------------------------------------------------------------

data Action = Move Integer       -- move back something
            | Done               -- finished
            | Abandon            -- abandoned
--            | Recurring        -- Completed a recurring task

instance Show Action where
  show (Move n)  = show n
  show Done      = "D"
  show Abandon   = "A"
--  show Recurring = "R"

instance Read Action where
  readsPrec n str0 =
          [ (Move n,str1)
          | (n, str1) <- readsPrec n str0
          ] ++
          [ (Move 1,str1)
          | ('+' : str1) <- [str0]
          ] ++
          [ (Done,str1)
          | ('D':str1) <- [str0]
          ] ++
          [ (Abandon,str1)
          | ('A':str1) <- [str0]
          ] ++
          []
--          [ (Recurring,str1)
--          | ('R':str1) <- [str0]
--          ]

--------------------------------------------------------------------

--data DueDate = DueDate Day

--------------------------------------------------------------------

data Line = Header Day
          | Item   Task
  deriving Show

-- You pass in the date today, so that the headers can have be more informative.
showLine :: Day -> Line -> String
showLine today (Header day)    = ljust 72 '-' ("--( " ++ showLongDay today day ++ " )" )
showLine today (Item task) = showTask task

instance Read Line where
  readsPrec _ str0 = first
         [ (Header day,"")
         | (comm,str1)   <- [ span (`elem` "-") str0]
         , length comm >= 2     -- 2 or more -'s
         , xs <- scanr (:) [] str1      -- *ANY* date anywhere is good
         , (day,str1) <- readsDay xs
         ] ++
         [ (Item task, str1)
         | (task,str1) <- readsPrec 0 str0
         ]
    where first []    = []
          first (x:_) = [x]

--------------------------------------------------------------------

data Task = Task (Maybe Action) String -- (Maybe DueDate)
  deriving Show

showTask :: Task -> String
showTask (Task act task) = "" ++ act_txt ++ "\t" ++ unwords (words task)
  where act_txt = case act of Nothing -> ""
                              Just t  -> show t

instance Read Task where
  readsPrec _ str0 =
         [ (Task (Just t) str2,"")
         | (t,str1)    <- readsPrec 0 str0
         , (sp,str2)   <- [span isSpace str1]
         , not (null sp)
         ] ++
         [ (Task Nothing str1,"")
         | (sp,str1)   <- [span isSpace str0]
         , not (null sp)        -- at least 1 space
         ]



--------------------------------------------------------------------

newtype TaskDB = TaskDB (Map Day [Task])
        deriving Show

showTaskDB :: Day -> TaskDB -> String
showTaskDB today (TaskDB db) = unlines
        [ showLine today ln
        | lns <- [ Header k : map Item ts
                 | (k,ts) <- M.toAscList db
                 , not (null ts)
                 ]
        , ln <- lns
        ]

readTaskDB :: String -> TaskDB
readTaskDB str0 = TaskDB $ M.fromListWith (flip (++)) $ parse today
        [ case readsPrec 0 txt of
           [(ln,"")] -> ln :: Line
           other -> error $ "readTaskDB failed on line  " ++ show n ++ " : " ++ show txt
        | (n,txt) <- [1..] `zip` lines str0
        , not (all isSpace txt)
        ]
 where
        parse date (Header date':rest) = parse date' rest
        parse date (Item task:rest)    =  (date,[task]) : parse date rest
        parse date []                  = []

db = TaskDB (M.fromList [(today,[Task Nothing "Hello"]),(tomorrow,[Task (Just Done) "World"])])

--------------------------------------------------------------------

interpDB :: ((Day,Task) -> Maybe (Day,Task)) -> TaskDB -> TaskDB
interpDB f (TaskDB db) = TaskDB
                       $ M.fromListWith (flip (++))
                       [ (day',[task'])
                       | (day,tasks) <- M.toList db
                       , task <- tasks
                       , Just (day',task') <- [f (day,task)]
                       ]

addTaskDB :: (Day,Task) -> TaskDB -> TaskDB
addTaskDB (day,task) (TaskDB db) = TaskDB $ M.insertWith (++) day [task] db

interp :: (Day,Task) -> Maybe (Day,Task)
interp i@(_,Task Nothing _) = return i
interp (day,Task (Just t) txt) = case t of
        Move n  -> return (addDays n day,Task Nothing txt)      -- offset from do-date
        Done    -> Nothing       -- plz delete
        Abandon -> Nothing       -- plz delete

-- If the interp deletes it, then the archive picks it up.
archive :: (Day,Task) -> Maybe (Day,Task)
archive x = case interp x of
              Nothing -> return x
              Just {} -> Nothing

--------------------------------------------------------------------
-- Directory, File, and message.
commit :: FilePath -> String -> IO ()
commit file msg = do
  (ex,sto,ste) <- readProcessWithExitCode "git" [ "commit", file, "-m", msg ] ""
  case ex of
    ExitSuccess -> return ()
    ExitFailure _ ->
        -- CHECK that there is no diff?
        return ()

--------------------------------------------------------------------

main = do
  utc <- getCurrentTime
--  print utc
  tz <- getCurrentTimeZone
  let today = localDay $ utcToLocalTime tz utc
--  print today

  env <- getEnvironment
--  putStr $ unlines $ map show env

  let todo_dir = case lookup "TODO_TODAY" env of
                   Just dir -> dir
                   Nothing -> case lookup "HOME" env of
                                Just dir -> dir ++ "/.todo"
                                Nothing -> error "can not find TODO_TODAY or HOME"

--  print todo_dir
  ok <- doesDirectoryExist todo_dir
  unless ok $ do
    error $ "Can not find directory : " ++ show todo_dir

  changeWorkingDirectory todo_dir

  let todo_file = "TODO"
  ok <- doesFileExist todo_file
  unless ok $ do
    error $ "Can not find file : " ++ show todo_file ++ ", in " ++ show todo_dir

  txt <- readFile todo_file
  let db = readTaskDB txt
--  print db

  let onDay off i@(d,_) | d == addDays off today = return i
                        | otherwise              = Nothing

  xs <- getArgs
  -- Output only
  let showing db  = putStrLn $ showTaskDB today db
  -- Writes a new copy
  let updating db = do
          commit todo_file "before GC"
--          print db
          renameFile todo_file (todo_file ++ ".bk")
          writeFile todo_file $ showTaskDB today db
          commit todo_file "after GC"

-- For now
--          renameFile todo_file todo_archive
--          renameFile todo_update todo_file

  -- Adding an entry to the file
  let adding n xs = updating $ addTaskDB (addDays n today,Task Nothing (unwords xs)) db

  let gc db = do
--          print db
          let new_db = ()
              arch_db@(TaskDB aDB) = interpDB archive db
--          appendFile todo_done $ showTaskDB (ModifiedJulianDay 0) arch_db
          updating $ interpDB interp db
          putStrLn $ "Archived " ++ show (M.size aDB) ++ " task(s)"
          return ()

  let edit = do
          commit todo_file "before edit"
          callProcess "emacs" [todo_file]
          commit todo_file "after edit"
          -- TODO: reload the db; call gc automatically

  case xs of
   []                  -> showing $ interpDB (onDay 0)
                                  $ db
   ["show"]            -> showing $ db
   [n] | all isDigit n && not (null n)
                      -> showing $ interpDB (onDay (read n))
                                 $ db
   ["edit"]           -> edit
   ["gc"]             -> gc db
   (n:rest) | all isDigit n && not (null n)
                      -> adding (read n) rest
   ("+":rest)         -> adding 0 rest
   other              -> error $ "I do not understand : " ++ show xs

--mainAdd :: Day -> String -> TaskDB -> IO ()
--mainAdd =


--------------------------------------------------------------------


today = fromGregorian 2014 1 8
tomorrow = addDays 1 today

