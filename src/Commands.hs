module Commands where

import Data.Functor (($>))
import Data.Maybe (isJust)
import Data.Time.Calendar (Day, fromGregorianValid)
import Text.Parsec
import Text.Parsec.String (Parser)

import Tasks

data Command
  = AddTask Description (Maybe Priority) (Maybe Day)
  | ViewTasks (Maybe Status)
  | MarkTask TaskId Status
  | DeleteTask TaskId
  | EditTask TaskId Description
  | Help
  | Quit
  deriving (Show)

commandParser :: Parser Command
commandParser = do
  spaces
  cmd <- addTaskParser
     <|> viewParser
     <|> markParser
     <|> deleteParser
     <|> editParser
     <|> helpParser
     <|> exitParser
  spaces
  eof
  return cmd

addTaskParser :: Parser Command
addTaskParser = do
  string "add" *> spaces
  desc  <- between (char '"') (char '"') (many1 (noneOf "\""))
 
  -- Prevent "-" consumption if only "-d" follows.
  pFlag <- optionMaybe $ try (spaces *> string "-p")

  -- Isolate priority parsing to avoid masking priority ParseError with "try".
  pri <- if isJust pFlag
    then optionMaybe (spaces *> priorityParser)
    else return Nothing

  due <- optionMaybe (spaces *> string "-d" *> many1 space *> dateParser)
  return $ AddTask desc pri due

viewParser :: Parser Command
viewParser = do
  string "view" *> spaces
  select <- optionMaybe (char '-' *> (char 'i' <|> char 'c'))
  case select of
    Just 'i' -> return $ ViewTasks (Just Todo)
    Just 'c' -> return $ ViewTasks (Just Complete)
    _        -> return $ ViewTasks Nothing

markParser :: Parser Command
markParser = do
  string "mark" *> spaces
  taskId <- read <$> many1 digit <?> "taskId"
  spaces
  status <- (string "todo" $> Todo) <|> (string "comp" $> Complete)
  return $ MarkTask taskId status

deleteParser :: Parser Command
deleteParser = do
  string "delete" *> spaces
  taskId <- read <$> many1 digit <?> "taskId"
  return $ DeleteTask taskId

editParser :: Parser Command
editParser = do
  string "edit" *> spaces
  taskId <- read <$> many1 digit <?> "taskId"
  spaces
  desc <- between (char '"') (char '"') (many1 (noneOf "\"")) <?> "quotation mark"
  return $ EditTask taskId desc

helpParser :: Parser Command
helpParser = string "-h" $> Help

exitParser :: Parser Command
exitParser = string "quit" $> Quit

priorityParser :: Parser Priority
priorityParser = (string "high" $> High)
    <|> (string "medium" $> Medium)
    <|> (string "low" $> Low)

dateParser :: Parser Day
dateParser = do
  y <- count 4 digit
  char '-'
  m <- count 2 digit
  char '-'
  d <- count 2 digit
  case fromGregorianValid (read y) (read m) (read d) of
    Just day -> return day
    Nothing  -> fail "Invalid date! Use this format: YYYY-MM-DD (without surrounding quot marks)"