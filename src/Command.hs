{-# LANGUAGE DeriveDataTypeable #-}
module Command where

import System.Console.CmdArgs

import Task




data TodoCmd = TodoCmd
                { done          :: Maybe NSWD
                , duration      :: String
                , description   :: [String]     -- as parsed by shell
                , by            :: String
                , do_           :: String
                }
              deriving (Show, Data, Typeable)

todoCmd = TodoCmd
            { done        = def   &= help "is this task Needed, Wanted, Should be done, or Done"
            , duration    = def   &= help "duration of time expected to take to completion"
            , description = def   &= args &= typ "THE TODO"
            , by          = def                 &= help "when this needs done by"
            , do_         = def   &= name "do"    &= help "when to do or review this"
            }

--          Add t -> Today t

parseCmds :: IO TodoCmd
parseCmds = cmdArgs (todoCmd &= program "todo-today")

