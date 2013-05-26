module DB where

import Data.Char
import Data.Map as Map
import System.Directory
import Data.List
import Control.DeepSeq
import System.IO

import Task
import Command

type DB = Map Int Task

loadDB :: String -> IO DB
loadDB dir_name = do
        dir <- getDirectoryContents dir_name
        contents <- sequence
                [ do h <- openFile (dir_name ++ "/" ++ f) ReadMode
                     txt <- hGetContents h
                     txt `deepseq` hClose h
                     return (read suff,readTask txt)
                | f <- dir
                , ".txt" `isSuffixOf` f
                , let suff = dropSuffix f
                , all isDigit suff
                , case suff of { '0':_ -> False ; _ -> True }
                ]
        return $ Map.fromList contents

dropSuffix xs | txt `isSuffixOf` xs = reverse $ drop (length txt) $ reverse xs
  where txt = ".txt"

newUniqDB :: DB -> Int
newUniqDB = succ . fst . findMax

writeDB :: String -> Int -> Task -> IO ()
writeDB dir_name i t = writeFile (dir_name ++ "/" ++ f) (showTask t)
  where
        f = show i ++ ".txt"
