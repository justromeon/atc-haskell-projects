module Todo where

import Data.Text (Text)
import Data.Time.Calendar (Day)

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