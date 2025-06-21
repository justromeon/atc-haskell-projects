{-# LANGUAGE OverloadedStrings #-}
module Database where

import Control.Monad (when)
import Data.List (intercalate)
import Data.Maybe (fromMaybe, catMaybes)
import Database.SQLite.Simple
    ( execute,
      execute_,
      query,
      query_,
      Only(Only),
      SQLData,
      Connection,
      Query(Query) )
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

idExists :: Connection -> TaskId -> IO Bool
idExists conn taskId = do
    result <- query conn "SELECT 1 FROM tasks WHERE id = ?" (Only taskId)
    return $ not $ null (result :: [Only Int])

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
        else do
            putStrLn "\nTask | Description                              | Status     | Priority | Due Date"
            putStrLn   "-----+------------------------------------------+------------+----------+-----------+"
            mapM_ print (tasks :: [Task])

setTaskComplete :: Connection -> TaskId -> IO ()
setTaskComplete conn taskId = do
    taskIdExists <- idExists conn taskId
    if taskIdExists
        then execute conn "UPDATE tasks SET status = ? WHERE id = ?" (Complete, taskId)
        else putStrLn $ "\nTask " ++ show (unTaskId taskId) ++ " does not exist"

deleteTask :: Connection -> TaskId -> IO ()
deleteTask conn taskId = do
    taskIdExists <- idExists conn taskId
    if taskIdExists
        then execute conn "DELETE FROM tasks WHERE id = ?" (Only taskId)
        else putStrLn $ "\nTask " ++ show (unTaskId taskId) ++ " does not exist"

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
    
    taskIdExists <- idExists conn taskId
    if taskIdExists
        then execute conn finalQuery updateValues
        else putStrLn ("\nTask " ++ show (unTaskId taskId) ++ " does not exist")

    when (null updateFields && taskIdExists) $ putStrLn "\nNo new values entered."