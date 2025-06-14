module Todo where

import Data.Char (toLower, isDigit)
import Data.Text (Text, pack)
import Data.Time.Calendar (Day)
import Data.Time (defaultTimeLocale, parseTimeM)

--Domain Model
newtype TaskId = TaskId Int
  deriving Show

newtype Description = Description Text
  deriving Show

data Status = Incomplete | Complete
  deriving Show

data Priority = Low | Medium | High
  deriving Show

data Task = Task
    { taskId      :: TaskId
    , description :: Description
    , status      :: Status
    , priority    :: Priority
    , dueDate     :: Maybe Day
    } deriving Show

--Smart Constructors
mkTaskId :: String -> Either String TaskId
mkTaskId s = case reads s of
  [(n,"")] | n >= 0 -> Right $ TaskId n
  [(n,"")]          -> Left "ID cannot be negative or zero"
  _                 -> Left $ "Contains non-digit characters: " ++ show (filter (not . isDigit ) s)

mkDesc :: String -> Either String Description
mkDesc s
    | null s    = Left "Description can't be empty."
    | otherwise = Right $ Description $ pack s

mkStatus :: String -> Either String Status
mkStatus s = case map toLower s of
  "incomplete" -> Right Incomplete
  "complete"   -> Right Complete
  _            -> Left $ "Invalid Status: " ++ s ++ " must be 'incomplete' or 'complete'"

mkPriority :: String -> Either String Priority
mkPriority s = case map toLower s of
    "low"    -> Right Low
    "medium" -> Right Medium
    "high"   -> Right High
    _        -> Left $ "Invalid priority: '" ++ s ++ "'. Must be one of: low, medium, high."

parseDueDate :: String -> Either String Day
parseDueDate s =
    case parseTimeM True defaultTimeLocale "%Y-%m-%d" s of
        Just day -> Right day
        Nothing  -> Left $ "Invalid date format: '" ++ s ++ "'. Please use YYYY-MM-DD."