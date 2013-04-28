module Task where

import Data.Time.Calendar

type TaskId = Int

data Task = Task
     { t_id	:: TaskId	        -- ^ Every task has a unique id
     , t_done	:: Bool 	        -- ^ Have you done this
     , t_dur    :: Maybe Int	        -- ^ How long, in minutes, to complete
     , t_desc 	:: String       	-- ^ Description of what needs done
     , t_by	:: Maybe TaskDay	-- ^ When must this be done by
     , t_do	:: Maybe TaskDay	-- ^ When do I plan to to/review this.
     }
     deriving (Show, Read)

newtype TaskDay = TaskDay Day

instance Show TaskDay where
        show (TaskDay day) = showGregorian day

instance Read TaskDay where
        -- replace with very liberal parser
        readsPrec _ str0 =
                [ (TaskDay $ fromGregorian year month day,str5)
                | (year, str1) <- reads str0
                , ("-",  str2) <- lex   str1
                , (month,str3) <- reads str2
                , ("-",  str4) <- lex   str3
                , (day,  str5) <- reads str4
                ]

