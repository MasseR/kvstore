module Main where

import Database.SQLite.Simple
import Lib

main :: IO ()
main = do
    conn <- open "test.db"
    startApp conn
