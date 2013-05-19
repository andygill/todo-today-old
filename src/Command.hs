{-# LANGUAGE DeriveDataTypeable #-}
module Command where

import System.Console.CmdArgs

import Task




data TodoCmd
          = Add { done          :: Maybe NSWD
                , duration      :: String
                , description   :: [String]     -- as parsed by shell
                , by            :: String
                , do_           :: String
                }
          | Mod { done          :: Maybe NSWD
                , duration      :: String
                , description   :: [String]     -- as parsed by shell
                , by            :: String
                , do_           :: String
                }
          | Dump                -- say everything
              deriving (Show, Data, Typeable)

dump  = Dump
add   = Add { done        = def   &= help "is this task Needed, Wanted, Should be done, or Done"
            , duration    = def   &= help "duration of time expected to take to completion"
            , description = def   &= args &= typ "THE TODO"
            , by          = def                 &= help "when this needs done by"
            , do_         = def   &= name "do"    &= help "when to do or review this"
            }
--          Add t -> Today t

parseCmds :: IO TodoCmd
parseCmds = cmdArgs (modes [add,dump])

