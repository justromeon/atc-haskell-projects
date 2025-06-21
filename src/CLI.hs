module CLI where

import Data.Char (toLower)
import Data.Time.Calendar (Day)
import Options.Applicative
    ( (<**>),
      Parser,
      ParserInfo,
      optional,
      argument,
      command,
      eitherReader,
      footer,
      header,
      help,
      info,
      long,
      metavar,
      option,
      progDesc,
      short,
      helper,
      hsubparser )

import Todo

data Command
  = AddTask
      { addDescription :: Description
      , addPriority    :: Maybe Priority
      , addDueDate     :: Maybe Day
      }
  | ViewTasks
      { viewStatusFilter :: Maybe Status
      , viewSortBy       :: Maybe SortKey
      }
  | CompleteTask TaskId
  | DeleteTask TaskId
  | EditTask
      { editTaskId      :: TaskId
      , editDescription :: Maybe Description
      , editStatus      :: Maybe Status
      , editPriority    :: Maybe Priority
      , editDueDate     :: Maybe Day
      }
  | Quit
  deriving Show

data SortKey = ByDueDate | ByPriority
  deriving (Show, Eq)

parseSortKey :: String -> Either String SortKey
parseSortKey s = case map toLower s of
  "duedate"  -> Right ByDueDate
  "priority" -> Right ByPriority
  _          -> Left $ "Invalid sort key: '" ++ s ++ "'. Must be 'duedate' or 'priority'."

addTaskParser :: Parser Command
addTaskParser = AddTask
  <$> argument (eitherReader mkDesc)
      (  metavar "DESCRIPTION"
      <> help "Description of the task" )
  <*> optional ( option (eitherReader mkPriority)
      (  long "priority"
      <> short 'p'
      <> metavar "PRIORITY"
      <> help "Task priority (low, medium, high)" ) )
  <*> optional ( option (eitherReader parseDueDate)
      (  long "due"
      <> short 'd'
      <> metavar "YYYY-MM-DD"
      <> help "Task due date (YYYY-MM-DD)" ) )

viewTasksParser :: Parser Command
viewTasksParser = ViewTasks
  <$> optional ( option (eitherReader mkStatus)
      (  long "status"
      <> short 's'
      <> metavar "STATUS"
      <> help "Filter by status (incomplete, complete)" ) )
  <*> optional ( option (eitherReader parseSortKey)
      (  long "sort-by"
      <> short 'b'
      <> metavar "KEY"
      <> help "Sort tasks by (duedate, priority)" ) )

completeTaskParser :: Parser Command
completeTaskParser = CompleteTask
  <$> argument (eitherReader mkTaskId)
      (  metavar "TASK_ID"
      <> help "ID of the task to complete" )

deleteTaskParser :: Parser Command
deleteTaskParser = DeleteTask
  <$> argument (eitherReader mkTaskId)
      (  metavar "TASK_ID"
      <> help "ID of the task to delete" )

editTaskParser :: Parser Command
editTaskParser = EditTask
  <$> argument (eitherReader mkTaskId)
      (  metavar "TASK_ID"
      <> help "ID of the task to edit"
      )
  <*> optional ( option (eitherReader mkDesc)
      (  long "desc"
      <> short 'D'
      <> metavar "DESCRIPTION"
      <> help "New description for the task" )
      )
  <*> optional ( option (eitherReader mkStatus)
      (  long "status"
      <> short 's'
      <> metavar "STATUS"
      <> help "New status for the task (incomplete, complete)" )
      )
  <*> optional ( option (eitherReader mkPriority)
      (  long "priority"
      <> short 'p'
      <> metavar "PRIORITY"
      <> help "New priority for the task (low, medium, high)" )
      )
  <*> optional ( option (eitherReader parseDueDate)
      (  long "due"
      <> short 'd'
      <> metavar "YYYY-MM-DD"
      <> help "New due date for the task (YYYY-MM-DD)" )
      )

quitParser :: Parser Command
quitParser = pure Quit

-- Main CLI Parser
commandParser :: Parser Command
commandParser = hsubparser
  (  command "add"      (info addTaskParser      $ progDesc "Add a new task")
  <> command "view"     (info viewTasksParser    $ progDesc "View tasks") 
  <> command "complete" (info completeTaskParser $ progDesc "Mark a task as complete") 
  <> command "delete"   (info deleteTaskParser   $ progDesc "Delete a task") 
  <> command "edit"     (info editTaskParser     $ progDesc "Edit a task") 
  <> command "quit"     (info quitParser         $ progDesc "Quits the program")
  )

commandParserInfo :: ParserInfo Command
commandParserInfo = info (commandParser <**> helper)
  (  header "-------HASKELL TODO APP-------"
  <> progDesc "A simple command-line todo application. To see all commands, use: --help."
  <> footer "To see options of a specific command, use: COMMAND --help"
  )
