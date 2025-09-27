{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- Backend for Todou app
--
-- A Todou is a list of Todos, in which is a list of entries. A todo
-- represents the todo list of a single day.
module Todou where

import Control.Applicative (Alternative((<|>)), asum)
import Control.Concurrent (threadDelay, forkIO, ThreadId, readMVar)
import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar, modifyMVar_)
import Control.Exception (try, SomeException, Exception (..))
import Control.Monad (unless, when, forM_, forever, join)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson (ToJSON(..), (.=), FromJSON(..), (.:))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Char (isSpace)
import Data.FileEmbed qualified as FileEmbed
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (stripPrefix)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Lazy qualified as LText
import Data.Time (Day, defaultTimeLocale, parseTimeM, formatTime, getCurrentTime, UTCTime (..))
import Debug.Trace (traceShow)
import Lucid (Html, head_, meta_, div_, link_, title_, body_, rel_, href_, httpEquiv_, content_, charset_, lang_, name_, html_, id_, script_, src_, type_)
import Lucid qualified
import Network.HTTP.Types (status500)
import Network.Wai.Middleware.RequestLogger (logStdout)
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (getArgs)
import System.FilePath (takeExtension, (</>), takeBaseName)
import Text.Read (readMaybe)
import Web.Scotty (get, scotty, html, raw, setHeader, post, Parsable(..), json, ActionM, body, captureParam, status, text, middleware, delete, redirect, put, queryParamMaybe, captureParamMaybe)


----------------------------------------
-- Constant
----------------------------------------

flushPeriod :: Int
flushPeriod = 5 * 1000000


----------------------------------------
-- Domain
----------------------------------------


-- | EntryId unique on each todou file per day. To universally
-- identify an entry you need date and the entryId.
newtype EntryId = EntryId Int deriving (Show, Eq, Ord)


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


-- | In memory buffer of the persisted todo data.
data TodoBuffer = TodoBuffer
  { todos       :: Map Day (Maybe Todo)
  , dirtyCounts :: Int
  }
  deriving Show


deleteEntry :: EntryId -> Todo -> Todo
deleteEntry entryId todo = todo { entries = filter (\e -> e.entryId /= entryId) todo.entries, dirty = True } :: Todo


updateEntry :: EntryId -> (Entry -> Entry) -> Todo -> Todo
updateEntry entryId f todo =
  todo { entries = fmap (\entry -> if entry.entryId == entryId then f entry else entry) todo.entries
       , dirty   = True
       }


updateTodo :: Day -> (Todo -> Todo) -> TodoBuffer -> TodoBuffer
updateTodo date f buffer =
 buffer
    { dirtyCounts = buffer.dirtyCounts + 1
    , todos = Map.update (\todo -> Just (mkDirty . f <$> todo)) date buffer.todos
    }
  where
    mkDirty todo = todo { dirty = True}


insertTodo :: Day -> Todo -> TodoBuffer -> TodoBuffer
insertTodo date todo buffer =
  buffer
    { dirtyCounts = buffer.dirtyCounts + 1
    , todos = Map.insert date (Just todo { dirty = True }) buffer.todos
    }


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
dumpEntry (Entry { entryId = EntryId entryId, description, completed }) = traceShow description $ Text.unlines
  [ "[entry]"
  , "id = " <> Text.pack (show entryId)
  , "description = " <> description
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


instance Parsable EntryId where
  parseParam s =
    case readMaybe (Text.unpack . LText.toStrict $ s) of
      Just n -> Right (EntryId n)
      Nothing -> Left "Invalid EntryId"


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
  FileSystemHandle :: FilePath -> MVar TodoBuffer -> Handle FileSystem
  S3Handle         :: MVar TodoBuffer -> Handle S3
  Sqlite3Handle    :: MVar TodoBuffer -> Handle Sqlite3


data SomeHandle = forall a . SomeHandle (Handle a)


-- | Create a new handle with all state required to operate the storage.
createHandle :: StorageOption -> IO SomeHandle

createHandle (StorageFileSystem dir) = do
  files <- filter (\path -> takeExtension path == ".todou") <$> listDirectory dir
  let todos =
        foldr (\path acc -> do
                  let dateStr = takeBaseName path
                  case parseTimeM @Maybe @Day True defaultTimeLocale "%Y-%m-%d" dateStr of
                    Just day -> Map.insert day Nothing acc
                    Nothing -> acc
              )
              mempty
              files
  ref <- newMVar TodoBuffer { todos = todos, dirtyCounts = 0 }
  pure . SomeHandle $ FileSystemHandle dir ref

createHandle (StorageS3 _) = do
  pure . SomeHandle $ S3Handle undefined

createHandle (StorageSqlite3 _) = do
  pure . SomeHandle $ Sqlite3Handle undefined

createHandle StorageNull = error "impossible"


getTodoBufferMVar :: SomeHandle -> MVar TodoBuffer
getTodoBufferMVar (SomeHandle (FileSystemHandle _ todouBufferMvar )) = todouBufferMvar
getTodoBufferMVar (SomeHandle (S3Handle todouBufferMvar ))           = todouBufferMvar
getTodoBufferMVar (SomeHandle (Sqlite3Handle todouBufferMvar ))      = todouBufferMvar


-- | Load Todo if it's not already cached in TodoBuffer.
loadTodo :: SomeHandle -> Day -> IO (Maybe Todo)

loadTodo (SomeHandle (FileSystemHandle dir todouBufferMvar)) date = do
  modifyMVar_ todouBufferMvar \buffer -> do
    case Map.lookup date buffer.todos of
      Just (Just _) -> pure buffer
      Just Nothing -> do
        let dateStr = formatTime defaultTimeLocale  "%Y-%m-%d" date
        let path = dir </> dateStr <> ".todou"
        mTodo <- parseTodo <$> Text.readFile path
        case mTodo of
          Just todo -> pure do
            buffer { todos       = Map.alter (\_ -> Just (Just todo)) date buffer.todos
                   , dirtyCounts = buffer.dirtyCounts + 1
                   }
          Nothing -> pure buffer
      Nothing -> pure buffer
  buffer <- readMVar todouBufferMvar
  pure do
    join (Map.lookup date buffer.todos)

loadTodo (SomeHandle (S3Handle todouBufferMvar)) date = do
  undefined

loadTodo (SomeHandle (Sqlite3Handle todouBufferMvar)) date = do
  undefined



-- | Flush in memory todo to storage
flush :: SomeHandle -> IO ()

flush (SomeHandle (FileSystemHandle dirPath todouBufferMvar)) = do
  modifyMVar_ todouBufferMvar \buffer -> do
    unless (buffer.dirtyCounts == 0) do
      forM_ buffer.todos \case
        Nothing -> pure ()
        Just (Todo { dirty = False }) -> pure ()
        Just todo@(Todo { date, dirty = True }) -> do
          let dateStr = formatTime defaultTimeLocale  "%Y-%m-%d" date
          let path = dirPath </> dateStr <> ".todou"
          Text.writeFile path (dumpTodo todo)
    pure $ buffer
      { dirtyCounts = 0
      , todos = (fmap . fmap) (\todo -> todo { dirty = False }) buffer.todos
      }

flush (SomeHandle (S3Handle _)) = undefined

flush (SomeHandle (Sqlite3Handle _)) = undefined


----------------------------------------
-- Daemon
----------------------------------------


spawnFlusher :: SomeHandle -> IO ThreadId
spawnFlusher handle = forkIO . forever $ do
  result <- try  do flush handle
  case result of
    Left (e :: SomeException) -> putStrLn (displayException e)
    Right _ -> pure ()
  threadDelay flushPeriod


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


-- | Frontend initial model
data Model = Model
  { entries    :: [Entry]
  , visibility :: Text
  , field      :: Text
  , nextId     :: EntryId
  , date       :: Text
  }


instance ToJSON Model where
  toJSON model = Aeson.object
    [ "entries"    .= model.entries
    , "visibility" .= model.visibility
    , "field"      .= model.field
    , "nextId"     .= model.nextId
    , "date"       .= model.date
    ]


todoToModel :: Todo -> Model
todoToModel todo =
  Model
    { entries    = todo.entries
    , visibility = "All"
    , field      = ""
    , nextId     = EntryId (lastId + 1)
    , date       = Text.pack (formatTime defaultTimeLocale  "%Y-%m-%d" todo.date)
    }
  where
    EntryId lastId
      | null todo.entries = EntryId 0
      | otherwise        = maximum (fmap (.entryId) todo.entries)


javascript :: ByteString -> ActionM ()
javascript bytes = do
    setHeader "Content-Type" "application/javascript"
    raw . ByteString.fromStrict $ bytes


css :: ByteString -> ActionM ()
css bytes = do
    setHeader "Content-Type" "text/css"
    raw . ByteString.fromStrict $ bytes


index :: Model -> Html ()
index model = do
  html_ [ lang_ "en" ] do
    head_ do
      meta_ [ charset_ "UTF-8" ]
      meta_ [ name_ "viewport", content_ "width=device-width, initial-scale=1.0, viewport-fit=cover" ]
      meta_ [ httpEquiv_ "X-UA-Compatible", content_ "ie=edge" ]
      link_ [ rel_ "stylesheet", href_ "main.css" ]
      title_ "Toudo"
    body_ do
      div_ [ id_ "app" ] mempty
      script_ [ id_ "model" ] (Aeson.encode model)
      script_ [ src_ "main.js", type_ "module" ] (mempty @Text)



server :: Options -> SomeHandle -> IO ()
server Options { port } handle = scotty port do
  middleware logStdout
  get "/" do
    today <- utctDay <$> liftIO getCurrentTime
    redirect ("/" <> LText.pack (formatTime defaultTimeLocale  "%Y-%m-%d" today))


  -- render the todo data for one date.
  get "/:date" do
    date <- captureParam @Day "date"
    let todouBufferMvar = getTodoBufferMVar handle
    buffer <- liftIO do
      flush handle -- flush on refresh
      takeMVar todouBufferMvar
    case Map.lookup date buffer.todos of
      Just (Just todo) -> do
        liftIO $ putMVar todouBufferMvar buffer
        html . Lucid.renderText $ index (todoToModel todo)
      Just Nothing -> do
        liftIO $ putMVar todouBufferMvar buffer
        liftIO (loadTodo handle date) >>= \case
          Just todo -> html . Lucid.renderText $ index (todoToModel todo)
          Nothing -> do
            status status500
            text "Can't find the todo data"
      Nothing -> do -- not in storage, create an empty one
        let newTodo = Todo { entries = [], date = date, dirty = True }
        liftIO $ putMVar todouBufferMvar buffer
        html . Lucid.renderText $ index (todoToModel newTodo)


  get "/main.css" do css $(FileEmbed.embedFile "data/todou/main.css")


  get "/main.js" do javascript $(FileEmbed.embedFile "data/todou/main.js")


  get "/vdom.js" do javascript $(FileEmbed.embedFile "data/todou/vdom.js")


  -- add a new entry
  post "/entry/add/:date/:id" do
    date        <- captureParam @Day "date"
    entryId     <- captureParam @EntryId "id"
    description <- Text.decodeUtf8 . ByteString.toStrict <$> body
    let newEntry =
          Entry
            { entryId     = entryId
            , description = description
            , completed   = False
            }
    liftIO $ loadTodo handle date >>= \case
      Just todo -> do -- update existing todo
        let newTodo = todo
              { entries = todo.entries <> [newEntry]
              , dirty   = True
              }
        modifyMVar_ (getTodoBufferMVar handle) (pure . updateTodo date (const newTodo))
      Nothing -> do -- create new todo if necessary
        let newTodo = Todo
              { entries = [newEntry]
              , date    = date, dirty = True
              }
        modifyMVar_ (getTodoBufferMVar handle) (pure . insertTodo date newTodo)
    json (Ok ())


  -- update an entry
  put "/entry/update/:date/:id" do
    date         <- captureParam @Day "date"
    mEntryId     <- captureParamMaybe @EntryId "id"
    mCompleted   <- queryParamMaybe "completed"
    mDescription <- queryParamMaybe @Text "description"
    let toNewEntry e = e
          { completed   = fromMaybe e.completed mCompleted
          , description = fromMaybe e.description mDescription
          }
    case mEntryId of
      Just entryId -> do
        hasChecked <- liftIO $ loadTodo handle date >>= \case
          Just _ -> do
            modifyMVar_ (getTodoBufferMVar handle)
              $ pure
              . updateTodo date (updateEntry entryId toNewEntry)
            pure True
          Nothing -> pure False
        if hasChecked
           then json (Ok ())
           else json (Err @Text "todo data doesn't exist")
      Nothing -> do
        hasChecked <- liftIO $ loadTodo handle date >>= \case
          Just _ -> do
            modifyMVar_ (getTodoBufferMVar handle)
              (pure . updateTodo date (\todo -> todo { entries = fmap toNewEntry todo.entries }))
            pure True
          Nothing -> pure False
        if hasChecked
           then json (Ok ())
           else json (Err @Text "todo data doesn't exist")


  -- delete an entry
  delete "/entry/delete/:date/:id" do
    date    <- captureParam @Day "date"
    entryId <- captureParam @EntryId "id"
    hasDeleted <- liftIO $ loadTodo handle date >>= \case
      Just _ -> do
        modifyMVar_ (getTodoBufferMVar handle) (pure . updateTodo date (deleteEntry entryId))
        pure True
      Nothing -> pure False
    if hasDeleted
       then json (Ok ())
       else json (Err @Text "can't find matching todo data")


  -- delete completed entries
  delete "/entry/delete/:date" do
    date      <- captureParam @Day "date"
    completed <- queryFlag "completed"
    hasDeleted <- liftIO $ loadTodo handle date >>= \case
      Just _
        | completed -> do
            modifyMVar_ (getTodoBufferMVar handle)
              (pure . updateTodo date (\todo -> todo { entries = filter (not . (.completed)) todo.entries } ))
            pure True
        | otherwise -> pure False
      Nothing -> pure False
    if hasDeleted
       then json (Ok ())
       else json (Err @Text "nothing is deleted")


----------------------------------------
-- Extended
----------------------------------------


queryFlag :: LText.Text -> ActionM Bool
queryFlag name = do
  mVal <- queryParamMaybe name :: ActionM (Maybe Text)
  pure $ case mVal of
    Just ""     -> True
    Just "true" -> True
    Just "1"    -> True
    _           -> False


----------------------------------------
-- Main
----------------------------------------


main :: IO ()
main = do
  options <- getArgs >>= checkArgs . parseArgs
  handle <- createHandle options.storage
  _ <- spawnFlusher handle
  server options handle
