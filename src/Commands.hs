module Commands where

import Data.Functor (($>))
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
  | Exit
  deriving (Show)

commandParser :: Parser Command
commandParser =
  addTaskParser
  <|> viewParser
  <|> markParser
  <|> deleteParser
  <|> editParser
  <|> helpParser
  <|> exitParser

addTaskParser :: Parser Command
addTaskParser = do
  string "add" *> spaces
  desc <- between (char '"') (char '"') (many1 anyChar)
  pri <- optionMaybe $ try (spaces *> string "-p" *> priorityParser)
  due <- optionMaybe $ try (spaces *> string "-d" *> spaces *> dateParser)
  return $ AddTask desc pri due

viewParser :: Parser Command
viewParser = do
  string "view" *> spaces
  select <- optionMaybe (try (string "-i") <|> string "-c")
  case select of
    Just "-i" -> return $ ViewTasks (Just Todo)
    Just "-c" -> return $ ViewTasks (Just Done)
    Nothing   -> return $ ViewTasks Nothing

markParser :: Parser Command
markParser = do
  string "mark" *> spaces
  taskId <- read <$> many1 digit
  status <- (string "todo" $> Todo) <|> (string "done" $> Done)
  return $ MarkTask taskId status

deleteParser :: Parser Command
deleteParser = do
  string "delete" *> spaces
  taskId <- read <$> many1 digit
  return $ DeleteTask taskId

editParser :: Parser Command
editParser = do
  string "edit" *> spaces
  taskId <- read <$> many1 digit
  spaces
  desc <- between (char '"') (char '"') (many1 anyChar)
  return $ EditTask taskId desc

helpParser :: Parser Command
helpParser = string "-h" $> Help

exitParser :: Parser Command
exitParser = string "exit" $> Exit

priorityParser :: Parser Priority
priorityParser = (string "high" $> High)
    <|> (string "medium" $> Medium)
    <|> (string "low" $> Low)

dateParser :: Parser Day
dateParser = do
  y <- many1 digit
  char '-'
  m <- many1 digit
  char '-'
  d <- many1 digit
  case fromGregorianValid (read y) (read m) (read d) of
    Just day -> return day
    Nothing  -> fail "Invalid date"