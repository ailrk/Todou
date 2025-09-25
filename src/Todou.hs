{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
module Todou where

import Control.Applicative (Alternative((<|>)))
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.Char (isSpace)
import Data.FileEmbed qualified as FileEmbed
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (stripPrefix)
import Data.Map qualified as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import System.Directory (doesDirectoryExist)
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Web.Scotty (get, scotty)


newtype EntryId = EntryId Int deriving (Show)


data Entry = Entry
  { id          :: EntryId
  , description :: Text
  , completed   :: Bool
  }
  deriving (Show)


-- | Todo entries for a single day
data Todo = Todo
  { entries :: [Entry]
  , date :: Day
  }
  deriving (Show)


data IniSection
  = EntrySection Entry
  | MetaSection Day


data Options = Options
  { dir  :: FilePath
  , port :: Int
  }
  deriving (Show)


defaultOptions :: Options
defaultOptions = Options { dir = "", port = 0 }


parseArgs :: [String] -> Options
parseArgs args
  = args
  & foldr
    (\s opts ->
        case stripPrefix "--dir=" s of
          Just dir -> opts { dir = dir }
          Nothing  ->
            case stripPrefix "--port=" s of
              Just port -> opts { port = fromMaybe (error "port needs to be a number") (readMaybe port) }
              Nothing   -> opts
    )
    defaultOptions
  & (\opts ->
        if
           | opts.dir == ""                        -> error "invalid storage directory"
           | opts.port < 1024 || opts.port > 65535 -> error ("invalid port number " <> show opts.port)
           | otherwise                             -> opts
    )


trim :: Text -> Text
trim = Text.dropAround isSpace


parseLine :: Text -> Maybe (Text, Text)
parseLine line =
  case Text.breakOn "=" line of
    (k, v)
      | not (Text.null v) -> Just (trim k, trim (Text.drop 1 v))
    _ -> Nothing


parseEntry :: [Text] -> Maybe Entry
parseEntry ls = do
  kvs         <- traverse parseLine ls
  entryId     <- lookup "id" kvs >>= readMaybe . Text.unpack <&> EntryId
  description <- lookup "description" kvs

  completed   <- do
    s <- lookup "completed" kvs
    case s of
      "true" -> pure True
      "false" -> pure False
      _ -> Nothing
  pure Entry
    { id = entryId
    , description = description
    , completed = completed
    }


parseMeta :: [Text] -> Maybe Day
parseMeta ls = do
  kvs <- traverse parseLine ls
  lookup "date" kvs >>= parseTimeM @Maybe @Day True defaultTimeLocale "%Y-%m-%d" . Text.unpack


-- | Split a file into list of entries
splitSections  :: [Text] -> [[Text]]
splitSections = snd . foldr
  (\line (buf, result) ->
    let stripped = Text.strip line
     in
      if "[" `Text.isPrefixOf` stripped
         then ([], filter (/= mempty) buf:result)
         else (line:buf, result)
    )
  ([], [])


parseSection :: [Text] -> Maybe IniSection
parseSection ls = do
  EntrySection <$> parseEntry ls
  <|> MetaSection <$> parseMeta ls


parseIni :: Text -> Maybe [IniSection]
parseIni txt = traverse parseSection (splitSections . Text.lines  $ txt)


parseTodo :: Text -> Maybe Todo
parseTodo txt = do
  sections <- parseIni txt
  let (entries, mDate) = foldr (\section (es, md)->
        case section of
          EntrySection e   -> (e:es, md)
          MetaSection date -> (es, Just date)
        )
        ([], Nothing)
        sections
  date <- mDate
  pure Todo
    { entries = entries
    , date    = date
    }


staticFiles :: Map FilePath ByteString
staticFiles = Map.fromList
  $(FileEmbed.embedDir "data/todou")


server :: Options -> IO ()
server Options { dir, port } = scotty port do
  get "/" do
    pure undefined


main :: IO ()
main = do
  options <- parseArgs <$> getArgs

  do
    dirExists <- doesDirectoryExist options.dir
    unless dirExists do
      error ("invalid options, storage directory " <> options.dir <> " is invalid")

  server options
