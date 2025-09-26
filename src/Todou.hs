{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- Backend for Todou app
--
-- A Toudu is a list of Todos, in which is a list of entries. A todo
-- represents the todo list of a single day.
module Todou where

import Control.Applicative (Alternative((<|>)), asum)
import Control.Monad (unless, when, forM_)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson (ToJSON(..), (.=), FromJSON(..), (.:))
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as ByteString
import Data.Char (isSpace)
import Data.FileEmbed qualified as FileEmbed
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.IORef (IORef, readIORef, newIORef, modifyIORef')
import Data.List (stripPrefix)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Lazy qualified as LText
import Data.Time (Day, defaultTimeLocale, parseTimeM, formatTime)
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (getArgs)
import System.FilePath (takeExtension, (</>))
import Text.Read (readMaybe)
import Web.Scotty (get, scotty, html, raw, setHeader, post, queryParam, Parsable(..), json, ActionM, body)
import Data.ByteString (ByteString)


----------------------------------------
-- Domain
----------------------------------------


newtype EntryId = EntryId Int deriving (Show)


instance ToJSON EntryId where
  toJSON (EntryId entryId) = toJSON entryId


instance FromJSON EntryId where
  parseJSON = fmap EntryId . parseJSON @Int


data Entry = Entry
  { entryId     :: EntryId
  , description :: Text
  , completed   :: Bool
  }
  deriving (Show)


instance ToJSON Entry where
  toJSON (Entry { entryId, description, completed }) =
    Aeson.object
      [ "id"          .= entryId
      , "description" .= description
      , "completed"   .= completed
      ]


instance FromJSON Entry where
  parseJSON = Aeson.withObject "Entry" \o -> do
    Entry
      <$> (o .: "id")
      <*> (o .: "description")
      <*> (o .: "completion")


-- | Todo entries for a single day
data Todo = Todo
  { entries :: [Entry]
  , date :: Day
  , dirty :: Bool -- indicate if the Todo is modified.
  }
  deriving (Show)


instance ToJSON Todo where
  toJSON (Todo { entries, date }) =
    Aeson.object
      [ "date"    .= formatTime defaultTimeLocale  "%Y-%m-%d" date
      , "entries" .= entries
      ]


instance FromJSON Todo where
  parseJSON = Aeson.withObject "Todo" \o -> do
    Todo
      <$> o .: "entries"
      <*> o .: "date"
      <*> pure True -- if the client sends us a Todo it must be dirty


-- | In memory representation of the todo storage layer.
data Toudu = Toudu
  { todos       :: Map Day (Maybe Todo)
  , dirtyCounts :: Int
  }
  deriving Show


dumpTodo :: Todo -> Text
dumpTodo (Todo { entries, date })
  = Text.unlines
  . mconcat
  $
    [ [ dumpDate date ]
    , fmap dumpEntry entries
    ]


dumpDate :: Day -> Text
dumpDate date = Text.unlines
  [ "[date]"
  , Text.pack ("date = " <> formatTime defaultTimeLocale  "%Y-%m-%d" date)
  ]


dumpEntry :: Entry -> Text
dumpEntry (Entry { entryId = EntryId entryId, description, completed })= Text.unlines
  [ "[entry]"
  , "id =" <> Text.pack (show entryId)
  , "description = \"" <> description <> "\""
  , "completed = " <> if completed then "true" else "false"
  ]


----------------------------------------
-- Parsing
----------------------------------------


data IniSection
  = EntrySection Entry
  | MetaSection Day


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
    { entryId = entryId
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
    , dirty   = False
    }


----------------------------------------
-- Options
----------------------------------------


data StorageOption
  = StorageFileSystem FilePath
  | StorageS3 Bucket
  | StorageSqlite3 ConnectionString
  | StorageNull
  deriving (Show, Eq)


data Options = Options
  { port    :: Int
  , storage :: StorageOption
  }
  deriving (Show)


defaultOptions :: Options
defaultOptions = Options { storage = StorageNull, port = 0 }


-- | Parse cli argument.
parseArgs :: [String] -> Options
parseArgs args
  = args
  & foldr
    (\s opts ->
        case stripPrefix "--storage=" s of
          Just arg -> do
            let storage = fromMaybe (error ("unknown storage argument " <> arg)) $
                  asum
                    [ stripPrefix "dir:"     arg <&> StorageFileSystem
                    , stripPrefix "s3:"      arg <&> StorageS3
                    , stripPrefix "sqlite3:" arg <&> StorageSqlite3
                    ]
            opts { storage = storage }
          Nothing  ->
            case stripPrefix "--port=" s of
              Just port -> opts { port = fromMaybe (error "port needs to be a number") (readMaybe port) }
              Nothing   -> opts
    )
    defaultOptions


-- | Argument sanity check.
checkArgs :: Options -> IO Options
checkArgs options = do
  case options.storage of
    StorageNull -> error "need a storage backend with --storage="
    StorageFileSystem dir -> do
      dirExists <- doesDirectoryExist dir
      unless dirExists do
        error ("invalid options, storage directory " <> dir <> " is invalid")
    StorageS3 _ -> error "not implemented"
    StorageSqlite3 _ -> error "not implemented"
  when (options.port < 1024 || options.port > 65535) do
    error ("invalid port number " <> show options.port)
  pure options


----------------------------------------
-- Orphan
----------------------------------------


instance Parsable Day where
  parseParam s =
    case parseTimeM @Maybe @Day True defaultTimeLocale "%Y-%m-%d" (LText.unpack s) of
      Just date -> Right date
      Nothing -> Left  "Invalid date format. expecting: %Y-%m-%d"


----------------------------------------
-- Handle
----------------------------------------


type Bucket = String
type ConnectionString = String


data HandleType
  = FileSystem
  | S3
  | Sqlite3


-- | Storage handle
data Handle (a :: HandleType) where
  FileSystemHandle :: FilePath -> IORef Toudu -> Handle FileSystem
  S3Handle         :: IORef Toudu -> Handle S3
  Sqlite3Handle    :: IORef Toudu -> Handle Sqlite3


data SomeHandle = forall a . SomeHandle (Handle a)


-- | Create a new handle with all state required to operate the storage.
createHandle :: StorageOption -> IO SomeHandle

createHandle (StorageFileSystem dir) = do
  files <- filter (\path -> takeExtension path == ".todou") <$> listDirectory dir
  let todos =
        foldr (\path acc ->
                  case parseTimeM @Maybe @Day True defaultTimeLocale "%Y-%m-%d" path of
                    Just day -> Map.insert day Nothing acc
                    Nothing -> acc
              )
              mempty
              files
  ref <- newIORef Toudu { todos = todos, dirtyCounts = 0 }
  pure . SomeHandle $ FileSystemHandle dir ref

createHandle (StorageS3 _) = do
  pure . SomeHandle $ S3Handle undefined

createHandle (StorageSqlite3 _) = do
  pure . SomeHandle $ Sqlite3Handle undefined

createHandle StorageNull = error "impossible"


getToudu :: SomeHandle -> IO Toudu
getToudu (SomeHandle (FileSystemHandle _ toudo )) = readIORef toudo
getToudu (SomeHandle (S3Handle toudo ))           = readIORef toudo
getToudu (SomeHandle (Sqlite3Handle toudo ))      = readIORef toudo


updateToudu :: SomeHandle -> (Toudu -> Toudu) -> IO ()
updateToudu (SomeHandle (FileSystemHandle _ toudo )) f = modifyIORef' toudo f
updateToudu (SomeHandle (S3Handle toudo ))           f = modifyIORef' toudo f
updateToudu (SomeHandle (Sqlite3Handle toudo ))      f = modifyIORef' toudo f


loadTodo :: SomeHandle -> Day -> IO (Maybe Todo)

loadTodo (SomeHandle (FileSystemHandle dir toudoRef)) date = do
  toudo <- readIORef toudoRef
  case Map.lookup date toudo.todos of
    Just (Just todo) -> pure (Just todo)
    Just Nothing -> do
      let dateStr = formatTime defaultTimeLocale  "%Y-%m-%d" date
      let path = dir </> dateStr </> ".toudo"
      parseTodo <$> Text.readFile path
    Nothing -> pure Nothing

loadTodo (SomeHandle (S3Handle toudo)) date = do
  undefined

loadTodo (SomeHandle (Sqlite3Handle toudo)) date = do
  undefined


-- | Flush in memory todo to storage
flush :: SomeHandle -> IO ()

flush handle@(SomeHandle (FileSystemHandle dirPath toudoRef)) = do
  do
    toudo <- readIORef toudoRef
    unless (toudo.dirtyCounts == 0) do
      forM_ toudo.todos \case
        Nothing -> pure ()
        Just (Todo { dirty = False }) -> pure ()
        Just todo@(Todo { date, dirty = True }) -> do
          let dateStr = formatTime defaultTimeLocale  "%Y-%m-%d" date
          let path = dirPath </> dateStr </> ".toudo"
          Text.writeFile path (dumpTodo todo)
  updateToudu handle \todou ->
    todou { dirtyCounts = 0
      , todos = (fmap . fmap) (\todo -> todo { dirty = False }) todou.todos
      }

flush (SomeHandle (S3Handle _)) = undefined

flush (SomeHandle (Sqlite3Handle _)) = undefined


----------------------------------------
-- Daemon
----------------------------------------


flusher :: SomeHandle -> IO ()
flusher = do
  undefined


----------------------------------------
-- Server
----------------------------------------


newtype Ok a = Ok a


instance ToJSON a => ToJSON (Ok a) where
  toJSON (Ok a) =
    Aeson.object
      [ "ok"   .= True
      , "data" .=  a
      ]

newtype Err a = Err a


instance ToJSON a => ToJSON (Err a) where
  toJSON (Err e) =
    Aeson.object
      [ "ok"   .= False
      , "err" .= e
      ]


javascript :: ByteString -> ActionM ()
javascript bytes = do
    setHeader "Content-Type" "application/javascript"
    raw . ByteString.fromStrict $ bytes


css :: ByteString -> ActionM ()
css bytes = do
    setHeader "Content-Type" "text/css"
    raw . ByteString.fromStrict $ bytes


server :: Options -> SomeHandle -> IO ()
server Options { port } handle = scotty port do
  get "/" do html . LText.fromStrict . Text.decodeUtf8 $ $(FileEmbed.embedFile "data/todou/index.html")

  get "/main.css" do css $(FileEmbed.embedFile "data/todou/main.css")

  get "/main.js" do javascript $(FileEmbed.embedFile "data/todou/main.js")

  get "/vdom.js" do javascript $(FileEmbed.embedFile "data/todou/vdom.js")

  get "todo" do
    date <- queryParam @Day "date"
    toudo <- liftIO $ getToudu handle
    case Map.lookup date toudo.todos of
      Just (Just todo) -> json (Ok todo)
      Just Nothing -> do
        liftIO (loadTodo handle date) >>= \case
          Just todo -> json (Ok todo)
          Nothing -> json (Err @Text "todo doesn't exist")
      Nothing -> json (Err @Text "todo doesn't exist")

  post "todo" do
    mTodo <- Aeson.decode @Todo <$> body
    case mTodo of
      Just todo -> do
        liftIO do
          updateToudu handle \todou ->
            todou
              { dirtyCounts = todou.dirtyCounts + 1
              , todos =
                  Map.update (\_ -> Just (Just todo))
                    todo.date
                    todou.todos
              }
        json (Ok ())

      Nothing -> do
        json (Err @Text "failed to parse, expect a Todo")


----------------------------------------
-- Main
----------------------------------------


main :: IO ()
main = do
  options <- getArgs >>= checkArgs . parseArgs
  handle <- createHandle options.storage
  server options handle
