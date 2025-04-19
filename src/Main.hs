{-# LANGUAGE OverloadedStrings #-}
module Main where

import Database.SQLite.Simple (close, execute_, open, Query)
import Text.Parsec (parse)

import Commands
import Database

main :: IO ()
main = do
  putStrLn "Enter a command. Use -h for help."
  input <- getLine
  processInput input

processInput :: String -> IO ()
processInput input =
  case parse commandParser "" input of
    Left err   -> do putStrLn $ "\n" ++ show err ++ "\n"
                     main
    Right Quit -> putStrLn "Closing Todo App.. Thankyou!"
    Right cmd  -> do executeCommand cmd
                     main

executeCommand :: Command -> IO ()
executeCommand cmd = do
  conn <- open "todo.db"
  execute_ conn schema
  case cmd of
    AddTask desc prioLevel dueDate -> insertTask conn desc prioLevel dueDate
    MarkTask taskId status         -> updateTaskMark conn taskId status
    ViewTasks filterMark           -> fetchTasks conn filterMark
    DeleteTask taskId              -> deleteTask conn taskId
    EditTask taskId newDesc        -> updateTaskDesc conn taskId newDesc
    Help                           -> displayHelpMenu
  close conn

displayHelpMenu :: IO ()
displayHelpMenu = putStrLn $ unlines [
    "Available commands:",
    "  add \"description\" [-p priority] [-d yyyy-mm-dd] - Add a new task.",
    "    -p: Priority (high, medium, low). Optional.",
    "    -d: Due date (yyyy-mm-dd). Optional.",
    "  view [-i|-c] - View tasks.",
    "    -i: View incomplete tasks.",
    "    -c: View completed tasks.",
    "  mark <taskId> <todo|comp> - Mark a task as todo or complete.",
    "  delete <taskId> - Delete a task.",
    "  edit <taskId> \"new description\" - Edit a task's description.",
    "  -h - Display this help menu.",
    "  quit - Quit the application."
  ]