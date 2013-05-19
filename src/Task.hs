{-# LANGUAGE DeriveDataTypeable #-}

module Task where

import Data.Data
import Data.Char
import Data.Default
import Data.Typeable
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import System.Locale
import Data.Row

type TaskId = Int

data NSWD = N | S | W | D
     deriving (Show, Read, Data, Typeable)

data Task = Task
     { t_done	:: !(Maybe NSWD)        -- ^ Have you done this, and do you want to
     , t_dur    :: !(Maybe Int)	        -- ^ How long, in minutes, to complete
     , t_by	:: !TaskDay	        -- ^ When must this be done by
     , t_do	:: !TaskDay	        -- ^ When do I plan to to/review this.
     , t_pri    :: !Rational            -- ^ Priority when sorting
     , t_task 	:: !Line              	-- ^ Description of what needs done
     }
     deriving (Show, Read, Data, Typeable)

instance Default Task where
        def = Task Nothing Nothing SomeDay SomeDay 0 (mkLine "")

instance Schema Task where
        fields = [ field "done" t_done $ \ f r -> r { t_done = f }
                 , field "dur"  t_dur  $ \ f r -> r { t_dur = f }
                 , field "by"   t_by   $ \ f r -> r { t_by = f }
                 , field "do"   t_do   $ \ f r -> r { t_do = f }
                 , field "pri"  t_pri  $ \ f r -> r { t_pri = f }
                 , field "task" t_task $ \ f r -> r { t_task = f }
                 ]

data TaskDay = TaskDay Day
             | SomeDay
     deriving (Data, Typeable)

instance Show TaskDay where
        show (TaskDay day) = showGregorian day
        show (SomeDay)     = "-"


y2k y | y < 50            = 2000 + y
      | y > 50 && y < 100 = 1900 + y
      | otherwise         = y


instance Read TaskDay where
        -- replace with very liberal parser
        readsPrec _ str0 =
                [ (TaskDay $ fromGregorian year month day,str5)
                | (year, str1) <- reads str0
                , ("-",  str2) <- lex   str1
                , (month,str3) <- reads str2
                , ("-",  str4) <- lex   str3
                , (day,  str5) <- reads str4
                ] ++
                [ (TaskDay $ fromGregorian (y2k year) month day,str5)
                | (month,str1) <- reads str0
                , ("/",  str2) <- lex   str1
                , (day,  str3) <- reads str2
                , ("/",  str4) <- lex   str3
                , (year, str5) <- reads str4
                ] ++
                [ (SomeDay,str1)
                | ("-",  str1) <- lex   str0
                ]

showDone :: Bool -> String
showDone True  = "X"
showDone False = "-"

showDuration :: Maybe Int -> String
showDuration Nothing = ""
showDuration (Just m) | m < 30    = show m ++ "m"
                      | otherwise = show (ceiling (fromIntegral m / 60)) ++ "h"
--        rjust 2 ' ' (show m) ++ "m"

showTaskDay :: Day -> TaskDay -> String
showTaskDay _today SomeDay = ""
showTaskDay today (TaskDay day)
        | diff == 0 = "Today"
        | diff == 1 = "Tomorrow"
        | diff > 0 && diff < 7 = the_day ++ "_" ++ show d ++ suff d
        | otherwise = rjust 2 '0' (show m) ++ "/" ++
                      rjust 2 '0' (show d) ++ "/" ++
                      rjust 2 '0' (show (y `mod` 100))
  where diff = diffDays day today
        (y,m,d) = toGregorian day
        the_day = formatTime defaultTimeLocale "%a" day
        suff 1 = "st"
        suff 2 = "nd"
        suff n = "th"


showNSWD :: Maybe NSWD -> String
showNSWD Nothing = ""
showNSWD (Just o) = show o

rjust :: Int -> Char -> String -> String
rjust n c xs = take n (take (n - length xs) (repeat c) ++ xs)


readTaskDay :: Day -> String -> Maybe TaskDay
readTaskDay today "" = return SomeDay
readTaskDay today "-" = return SomeDay
readTaskDay today str =
        case reads str of
         [(td,"")] -> return td
         _           ->
          case reads str of
           [(n,"")] -> return (TaskDay $ addDays n today)
           _ ->  Nothing

titleLine :: String
titleLine = "  Dur   Due-By     Do-On"

taskLine :: Day -> Task -> String
taskLine today t = rjust 1 ' ' (showNSWD (t_done t))
         ++ " " ++ rjust 3 ' ' (showDuration (t_dur t))
         ++ " " ++ rjust 9 ' ' (showTaskDay today (t_by t))
         ++ " " ++ rjust 9 ' ' (showTaskDay today (t_do t))
         ++ " : " ++ showLine (t_task t)


-- very liberal parser
readTask :: String -> Task
readTask str = case reads str of
                      [(Row t,xs)] | all isSpace xs -> t
                      _ -> def { t_task = mkLine str }

showTask :: Task -> String
showTask = show . Row

newtype Line = Line String
        deriving (Data, Typeable)

mkLine :: String -> Line
mkLine = Line . unwords . words

showLine :: Line -> String
showLine (Line str) = str

instance Show Line where show (Line str) = show str
instance Read Line where
    readsPrec n str = map (\ (a,b) -> (mkLine a,b)) $ readsPrec n str

--showRow :: Int -> Task -> String
--showRow =

todayIs :: IO Day
todayIs = do
        utc <- getCurrentTime
        return $ utctDay utc

main2 = do
        txt <- readFile "todo/2.txt"
        let t = readTask txt
        print t
        today <- todayIs
        putStrLn $ titleLine
        putStrLn (taskLine today t)
        let t2 = t { t_done = Just N
                   , t_dur = Just 400
                   , t_by = read "2012-01-01"
                   , t_do = read "2013-10-31"
                   , t_pri = 1.0
                   }
        putStrLn (taskLine today t2)