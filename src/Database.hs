{-# LANGUAGE OverloadedStrings #-}
module Database where

import Database.SQLite.Simple
import Data.Time (Day)

import Tasks

instance FromRow Task where
  fromRow = Task <$> field <*> field <*> fmap toEnum field <*> fmap toEnum field <*> field

schema :: Query
schema =
     "CREATE TABLE IF NOT EXISTS tasks ("
  <> "    task_id INTEGER PRIMARY KEY AUTOINCREMENT,"
  <> "    description TEXT NOT NULL,"
  <> "    completed INTEGER NOT NULL,"
  <> "    priority INTEGER,"
  <> "    due_date TEXT"
  <> ");"

insertTask :: Connection -> Description -> Maybe Priority -> Maybe Day -> IO ()
insertTask conn desc prioLevel dueDate = execute conn
  "INSERT INTO tasks (description, completed, priority, due_date) VALUES (?, ?, ?, ?)"
  (desc, False, priority, dueDate)
  where
    priority = maybe 0 fromEnum prioLevel

fetchTasks :: Connection -> Maybe Status -> IO ()
fetchTasks conn sortOption = do
  tasks <- case sortOption of
    Just Todo     -> query_ conn "SELECT * FROM tasks WHERE completed = 0 ORDER BY due_date IS NULL, due_date, priority DESC"
    Just Complete -> query_ conn "SELECT * FROM tasks WHERE completed = 1 ORDER BY due_date IS NULL, due_date, priority DESC"
    _             -> query_ conn "SELECT * FROM tasks ORDER BY due_date IS NULL, due_date, priority DESC"
  if null (tasks :: [Task])
    then putStrLn "No tasks found."
    else do
      putStrLn "Task | Description                              | Status    | Priority | Due Date"
      putStrLn "-----+------------------------------------------+-----------+----------+------------"
      mapM_ print tasks

updateTaskMark :: Connection -> TaskId -> Status -> IO ()
updateTaskMark conn taskId status = do
  execute conn "UPDATE tasks SET completed = ? WHERE task_id = ?" (fromEnum status, taskId)

deleteTask :: Connection -> TaskId -> IO ()
deleteTask conn taskId =
  execute conn "DELETE FROM tasks WHERE task_id = ?" (Only taskId)

updateTaskDesc :: Connection -> TaskId -> Description -> IO ()
updateTaskDesc conn taskId newDesc =
  execute conn "UPDATE tasks SET description = ? WHERE task_id = ?" (newDesc, taskId)