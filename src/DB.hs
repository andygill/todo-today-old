module DB where

import Data.Char
import Data.Map as Map
import System.Directory
import Data.List

import Task
import Command

type DB = Map Int Task

loadDB :: String -> IO DB
loadDB dir_name = do
        dir <- getDirectoryContents dir_name
        contents <- sequence
                [ do txt <- readFile (dir_name ++ "/" ++ f)
                     return (read suff,readTask txt)
                | f <- dir
                , ".txt" `isSuffixOf` f
                , let suff = dropSuffix f
                , all isDigit suff
                , case suff of { '0':_ -> False ; _ -> True }
                ]
        print contents
        return $ Map.fromList contents

dropSuffix xs | txt `isSuffixOf` xs = reverse $ drop (length txt) $ reverse xs
  where txt = ".txt"

newUniqDB :: DB -> Int
newUniqDB = succ . fst . findMax

writeDB :: String -> Int -> Task -> IO ()
writeDB dir_name i t = writeFile (dir_name ++ "/" ++ f) (showTask t)
  where
        f = show i ++ ".txt"
