{-# LANGUAGE OverloadedStrings #-}
module Database where

import Data.List (intercalate)
import Data.Maybe (fromMaybe, catMaybes)
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField ( ToField(toField) )
import Data.Time (Day)
import Data.Text (pack)

import CLI
import Todo

schema :: Query
schema =
       "CREATE TABLE IF NOT EXISTS tasks ("
    <> "id INTEGER PRIMARY KEY AUTOINCREMENT,"
    <> "description TEXT NOT NULL,"
    <> "status INTEGER NOT NULL,"
    <> "priority INTEGER NOT NULL,"
    <> "due_date TEXT"
    <> ")"

initializeDB :: Connection -> IO ()
initializeDB conn = execute_ conn schema

insertTask :: Connection -> Description -> Maybe Priority -> Maybe Day -> IO ()
insertTask conn desc prio due = do
    let finalPrio = fromMaybe Medium prio

    execute conn
      "INSERT INTO tasks (description, status, priority, due_date) VALUES (?, ?, ?, ?)"
      (desc, Incomplete, finalPrio, due)


displayTasks :: Connection -> Maybe Status -> Maybe SortKey -> IO ()
displayTasks conn stat sorter = do
    let partialQuery = "SELECT id, description, status, priority, due_date FROM tasks"
        whereClause  = case stat of
            Just Incomplete -> " WHERE status = 0"
            Just Complete   -> " WHERE status = 1"
            Nothing         -> ""
        orderClause  = case sorter of
            Just ByDueDate  -> " ORDER BY due_date IS NULL, due_date ASC"
            Just ByPriority -> " ORDER BY priority DESC, due_date IS NULL"
            Nothing         -> ""
    tasks <- query_ conn $ partialQuery <> whereClause <> orderClause
    if null tasks
        then putStrLn "\nNo Tasks found, database is empty."
        else mapM_ print (tasks :: [Task])

setTaskComplete :: Connection -> TaskId -> IO ()
setTaskComplete conn taskId = execute conn
    "UPDATE tasks SET status = ? WHERE id = ?" (Complete, taskId)

deleteTask :: Connection -> TaskId -> IO ()
deleteTask conn taskId = execute conn
    "DELETE FROM tasks WHERE id = ?" (Only taskId)

updateTask :: Connection
           -> TaskId
           -> Maybe Description
           -> Maybe Status
           -> Maybe Priority
           -> Maybe Day
           -> IO ()
updateTask conn taskId newDesc newStat newPrio newDue = do

    let updatesWithValues :: [(String, SQLData)]
        updatesWithValues = catMaybes
          [ fmap (\desc -> ("description = ?", toField desc) ) newDesc
          , fmap (\stat -> ("status = ?"     , toField stat) ) newStat
          , fmap (\prio -> ("priority = ?"   , toField prio) ) newPrio
          , fmap (\due  -> ("due_date = ?"   , toField due)  ) newDue
          ]

        updateFields = map fst updatesWithValues
        updateValues = map snd updatesWithValues ++ [toField taskId]
        finalQuery =  "UPDATE tasks SET "
                   <> Query (pack $ intercalate ", " updateFields)
                   <> " WHERE id = ?"
    
    if null updateFields
        then putStrLn $ "No new values entered. Task " ++ show (unTaskId taskId) ++ " stays the same."
        else execute conn finalQuery updateValues