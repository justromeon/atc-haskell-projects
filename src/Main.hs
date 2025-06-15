module Main where

import Database.SQLite.Simple
import Options.Applicative (execParser)

import CLI
import Database

executeCommand :: Command -> IO ()
executeCommand cmd = do
    conn <- open "todo.db"
    execute_ conn schema
    case cmd of
        AddTask desc prio day -> insertTask conn desc prio day
        ViewTasks stat sorter -> displayTasks conn stat sorter
        CompleteTask taskId   -> setTaskComplete conn taskId
        DeleteTask taskId     -> deleteTask conn taskId
        EditTask taskId d s p due -> updateTask conn taskId d s p due 
        Quit                  -> putStrLn "Closing the Todo App... Thankyou!"
    close conn

main :: IO ()
main = execParser commandParserInfo >>= executeCommand
     