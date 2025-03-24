module Tasks where

import Data.Time.Calendar (Day)
import Text.Printf (printf)

data Priority = Low 
              | Medium 
              | High 
              deriving (Show, Enum)

data Status = Todo 
            | Complete 
            deriving (Show, Enum)

type TaskId = Int
type Description = String

data Task = Task
  { taskId :: TaskId
  , description :: Description
  , completed :: Status
  , priority :: Priority
  , dueDate :: Maybe Day
  }

instance Show Task where
  show (Task tId desc comp prio due) =
    printf "%-4d | %-40s | %-9s | %-8s | %-10s"
      tId
      desc
      (show comp)
      (show prio)
      (show due)