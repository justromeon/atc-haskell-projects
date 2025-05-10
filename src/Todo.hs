module Todo where

import Data.Char (toLower)
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

data Priority = None | Low | Medium | High
  deriving Show

data Task = Task
    { taskId      :: Maybe TaskId
    , description :: Description
    , status      :: Status
    , priority    :: Priority
    , dueDate     :: Maybe Day
    } deriving Show

--Smart Constructors
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
    "none"   -> Right None
    "low"    -> Right Low
    "medium" -> Right Medium
    "high"   -> Right High
    _        -> Left $ "Invalid priority: '" ++ s ++ "'. Must be one of: none, low, medium, high."

parseDueDate :: String -> Either String Day
parseDueDate s =
    case parseTimeM True defaultTimeLocale "%Y-%m-%d" s of
        Just day -> Right day
        Nothing  -> Left $ "Invalid date format: '" ++ s ++ "'. Please use YYYY-MM-DD."