{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- Backend for Todou app
--
-- A Todou is a list of Todos, in which is a list of entries. A todo
-- represents the todo list of a single day.
module Todou where

import Amazonka qualified
import Amazonka.S3 qualified as Amazonka
import Amazonka.S3.GetObject qualified
import Amazonka.S3.ListObjectsV2 qualified as Amazonka
import Amazonka.S3.Types.Object (Object(..))
import Conduit qualified
import Control.Applicative (Alternative((<|>)), asum)
import Control.Concurrent (threadDelay, forkIO, ThreadId, readMVar)
import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar, modifyMVar_)
import Control.Exception (try, SomeException, Exception (..))
import Control.Monad (unless, when, forM_, forever, join, void)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson (ToJSON(..), (.=), FromJSON(..), (.:))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isSpace)
import Data.Coerce (coerce)
import Data.FileEmbed qualified as FileEmbed
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (stripPrefix, foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.String.Interpolate (iii, i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Lazy qualified as LText
import Data.Time
    ( Day,
      defaultTimeLocale,
      parseTimeM,
      formatTime,
      getCurrentTime,
      utcToLocalTime,
      utc,
      LocalTime(..), addDays )
import Database.SQLite.Simple (ToRow(..), FromRow(..), Only (..), type (:.) ((:.)), Query (..))
import Database.SQLite.Simple qualified as Sqlite
import Database.SQLite.Simple.FromField (FromField(..))
import Database.SQLite.Simple.ToField (ToField(..))
import Lucid (Html, head_, meta_, div_, link_, title_, body_, rel_, href_, httpEquiv_, content_, charset_, lang_, name_, html_, id_, script_, src_, type_, sizes_)
import Lucid qualified
import Network.HTTP.Types (status500)
import Network.URI qualified as URI
import Network.Wai.Middleware.RequestLogger (logStdout)
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (getArgs)
import System.Environment qualified as Environment
import System.FilePath (takeExtension, (</>), takeBaseName)
import Text.Read (readMaybe)
import Web.Scotty (get, scotty, html, raw, setHeader, post, Parsable(..), json, ActionM, body, captureParam, status, text, middleware, delete, redirect, put, queryParamMaybe, captureParamMaybe, header)
import Web.Cookie (parseCookies)
import Data.Bits (Bits(..))
import Data.Word (Word32)


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
data Buffer = Buffer
  { todos       :: Map Day (Maybe Todo)
  , dirtyCounts :: Int
  }
  deriving Show


pattern TodoNotExists :: Maybe (Maybe Todo)
pattern TodoNotExists = Nothing


pattern TodoNotLoaded :: Maybe (Maybe Todo)
pattern TodoNotLoaded = Just Nothing


pattern TodoLoaded :: Todo -> Maybe (Maybe Todo)
pattern TodoLoaded a = Just (Just a)
{-# COMPLETE TodoNotExists, TodoNotLoaded, TodoLoaded #-}


deleteEntry :: EntryId -> Todo -> Todo
deleteEntry entryId todo = todo { entries = filter (\e -> e.entryId /= entryId) todo.entries, dirty = True } :: Todo


updateEntry :: EntryId -> (Entry -> Entry) -> Todo -> Todo
updateEntry entryId f todo =
  todo { entries = fmap (\entry -> if entry.entryId == entryId then f entry else entry) todo.entries
       , dirty   = True
       }


updateTodo :: Day -> (Todo -> Todo) -> Buffer -> Buffer
updateTodo date f buffer =
 buffer
    { dirtyCounts = buffer.dirtyCounts + 1
    , todos = Map.update (\todo -> Just (mkDirty . f <$> todo)) date buffer.todos
    }
  where
    mkDirty todo = todo { dirty = True}


insertTodo :: Day -> Todo -> Buffer -> Buffer
insertTodo date todo buffer =
  buffer
    { dirtyCounts = buffer.dirtyCounts + 1
    , todos = Map.insert date (Just todo { dirty = True }) buffer.todos
    }


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
dumpEntry (Entry { entryId = EntryId entryId, description, completed }) = Text.unlines
  [ "[entry]"
  , "id = " <> Text.pack (show entryId)
  , "description = " <> description
  , "completed = " <> if completed then "true" else "false"
  ]


----------------------------------------
-- Domain.Sqlite3
----------------------------------------


instance ToField EntryId where
  toField (EntryId entryId) = toField entryId


instance FromField EntryId where
  fromField = fmap EntryId <$> fromField @Int


instance ToRow Entry where
  toRow (Entry { entryId, description, completed }) =
    toRow (entryId, description, completed)


instance FromRow Entry where
  fromRow = Entry <$> Sqlite.field <*> Sqlite.field <*> Sqlite.field


createSqliteSchema :: Sqlite.Connection -> IO ()
createSqliteSchema conn = do
  Sqlite.execute_ conn [i| PRAGMA foreign_keys = ON; |]
  Sqlite.execute_ conn [i| CREATE TABLE IF NOT EXISTS todo ( date TEXT PRIMARY KEY, UNIQUE(date)); |]
  Sqlite.execute_ conn [iii|
      CREATE TABLE IF NOT EXISTS entry
        ( id          INTEGER KEY NOT NULL
        , description TEXT NOT NULL
        , completed   BOOLEAN NOT NULL
        , todo_date   INTEGER NOT NULL
        , UNIQUE(id, todo_date)
        , FOREIGN KEY(todo_date) REFERENCES todo(date) ON DELETE CASCADE
        );
    |]
  Sqlite.execute_ conn [i| CREATE INDEX IF NOT EXISTS idx_entry_todo_date ON entry(todo_date); |]
  Sqlite.execute_ conn [i| CREATE INDEX IF NOT EXISTS idx_entry_todo_completed ON entry(todo_date, completed); |]


----------------------------------------
-- Domain.S3
----------------------------------------


createS3Env :: IO Amazonka.Env
createS3Env = do
  mUrl <- lookupAWSEndpointURL
  let setEndpointURL =
        case mUrl of
          Just url ->
            case url.uriAuthority of
              Nothing -> id
              Just auth -> do
                let host = Char8.pack auth.uriRegName
                let port = fromMaybe 443 . readMaybe $ auth.uriPort
                Amazonka.setEndpoint True host port
          Nothing -> id
  let service = setEndpointURL Amazonka.defaultService
  Amazonka.configureService service <$> Amazonka.newEnv Amazonka.discover
  where
    lookupAWSEndpointURL = do
      Environment.lookupEnv "AWS_ENDPOINT_URL" <&> \case
        Nothing -> Nothing
        Just "" -> Nothing
        Just v  -> URI.parseURI v


----------------------------------------
-- Options
----------------------------------------

type Bucket = Text
type ConnectionString = String


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
                    [ stripPrefix "dir:"    arg <&> StorageFileSystem
                    , stripPrefix "s3:"     arg <&> StorageS3 . Text.pack
                    , stripPrefix "sqlite:" arg <&> StorageSqlite3
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
    StorageS3 _ -> pure ()
    StorageSqlite3 _ -> pure ()
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


data Handle
  = FileSystemHandle FilePath (MVar Buffer)
  | Sqlite3Handle Sqlite.Connection (MVar Buffer)
  | S3Handle  Amazonka.Env Bucket (MVar Buffer)


-- | Create a new handle with all state required to operate the storage.
createHandle :: StorageOption -> IO Handle
createHandle options = do
  case options of
    StorageFileSystem dir -> do
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
      ref <- newMVar Buffer { todos = todos, dirtyCounts = 0 }
      pure $ FileSystemHandle dir ref

    StorageSqlite3 connStr -> do
      conn <- Sqlite.open connStr
      createSqliteSchema conn
      dates <- Sqlite.query_ @(Only Text) conn "SELECT date FROM todo"
      let todos =
            foldr (\(Only date) acc -> do
                      case parseTimeM @Maybe @Day True defaultTimeLocale "%Y-%m-%d" (Text.unpack date) of
                        Just day -> Map.insert day Nothing acc
                        Nothing -> acc
                  )
                  mempty
                  dates
      ref <- newMVar Buffer { todos = todos, dirtyCounts = 0 }
      pure $ Sqlite3Handle conn ref

    StorageS3 bucket -> do
      env <- createS3Env
      let request = Amazonka.newListObjectsV2 (Amazonka.BucketName bucket)
      resp <- Amazonka.runResourceT $ Amazonka.send env request
      let dates = fmap (\o -> coerce @_ @Text o.key) (fromMaybe []  resp.contents)
      let todos =
            foldr (\date acc -> do
                      case parseTimeM @Maybe @Day True defaultTimeLocale "%Y-%m-%d" (Text.unpack date) of
                        Just day -> Map.insert day Nothing acc
                        Nothing -> acc
                  )
                  mempty
                  dates
      ref <- newMVar Buffer { todos = todos, dirtyCounts = 0 }
      pure $ S3Handle env bucket ref

    StorageNull ->
      error "impossible"


getBufferMVar :: Handle -> MVar Buffer
getBufferMVar ((FileSystemHandle _ bufferMvar )) = bufferMvar
getBufferMVar ((Sqlite3Handle _ bufferMvar ))    = bufferMvar
getBufferMVar ((S3Handle _ _ bufferMvar ))       = bufferMvar


-- | Load Todo if it's not already cached in Buffer.
loadTodo :: Handle -> Day -> IO (Maybe Todo)
loadTodo handle date = do
  let bufferMVar = getBufferMVar handle
  modifyMVar_ bufferMVar \buffer -> do
    case Map.lookup date buffer.todos of
      TodoLoaded _ -> pure buffer
      TodoNotExists -> pure buffer
      TodoNotLoaded -> do
        let dateStr = formatTime defaultTimeLocale  "%Y-%m-%d" date
        case handle of
          FileSystemHandle dir _ -> do
            mTodo <- parseTodo <$> Text.readFile (dir </> dateStr <> ".todou")
            case mTodo of
              Just todo -> pure do
                buffer { todos       = Map.alter (\_ -> TodoLoaded todo) date buffer.todos
                       , dirtyCounts = buffer.dirtyCounts + 1
                       }
              Nothing -> pure buffer
          Sqlite3Handle conn _ -> do
            entries <- Sqlite.query conn
              [i|SELECT (id, description, completed) FROM entry WHERE todo_date = (?); |] (Only dateStr)
            let todo = Todo
                  { entries = entries
                  , date    = date
                  , dirty   = False
                  }
            pure do
              buffer { todos       = Map.alter (\_ -> TodoLoaded todo) date buffer.todos
                     , dirtyCounts = buffer.dirtyCounts + 1
                     }
          S3Handle env bucket _ -> do
            let request = Amazonka.newGetObject (Amazonka.BucketName bucket) (Amazonka.ObjectKey (Text.pack dateStr))
            chunks <- Amazonka.runResourceT do
              resp <- Amazonka.send env request
              Conduit.runConduit $ resp.body.body Conduit..| Conduit.sinkList
            let mTodo = parseTodo . Text.decodeUtf8 . LBS.toStrict . LBS.fromChunks $ chunks
            case mTodo of
              Just todo -> pure do
                buffer { todos       = Map.alter (\_ -> TodoLoaded todo) date buffer.todos
                       , dirtyCounts = buffer.dirtyCounts + 1
                       }
              Nothing -> pure buffer

  buffer <- readMVar bufferMVar
  pure do
    join (Map.lookup date buffer.todos)


-- | Get the presence map from the previous 30 days
getPresences :: Handle -> Day -> [Integer] -> IO Word32
getPresences handle date offsets = do
  results <- traverse (loadTodo handle) days30
  pure
    $ foldl' setIfTrue zeroBits
    $ zip [0..]
    $ fmap (isJust . cleanup) results
  where
    days30 = fmap (`addDays` date) offsets
    cleanup = \case
      Just (Todo { entries = []} ) -> Nothing
      Nothing -> Nothing
      other -> other

    setIfTrue acc (idx, True) = setBit acc idx
    setIfTrue acc (_, False) = acc


-- | Flush in memory todo to storage
flush :: Handle -> IO ()
flush handle = do
  let bufferMVar = getBufferMVar handle
  modifyMVar_ bufferMVar \buffer -> do
    unless (buffer.dirtyCounts == 0) do
      forM_ buffer.todos \case
        Nothing -> pure ()
        Just (Todo { dirty = False }) -> pure ()
        Just todo@(Todo { entries, date, dirty = True }) -> do
          let dateStr = formatTime defaultTimeLocale  "%Y-%m-%d" date
          case handle of
            FileSystemHandle dirPath _ -> do
              let path = dirPath </> dateStr <> ".todou"
              Text.writeFile path (dumpTodo todo)
            Sqlite3Handle conn _ -> do
              Sqlite.execute conn
                [iii|
                      INSERT INTO todo (date) VALUES (?)
                      ON CONFLICT(date) DO NOTHING;
                |] (Only date)
              let ids = fmap (.entryId) entries
              let placeholders = Query ("(" <> Text.intercalate "," (replicate (length ids) "?") <> ")")
              Sqlite.execute conn
                (" DELETE FROM entry where todo_date = (?) AND id NOT IN " <> placeholders)
                (Only date :. ids)
              Sqlite.executeMany conn
                [iii|
                      INSERT INTO entry (todo_date, id, description, completed) VALUES (?,?,?,?)
                      ON CONFLICT(id, todo_date) DO UPDATE SET
                        description = excluded.description,
                        completed   = excluded.completed;
                |]
                $ fmap (Only date :.) entries
            S3Handle env bucket _ -> do
              let req = Amazonka.newPutObject
                    (Amazonka.BucketName bucket)
                    (Amazonka.ObjectKey (Text.pack dateStr))
                    (Amazonka.toBody (dumpTodo todo))
              void . Amazonka.runResourceT $ Amazonka.send env req

    pure $ buffer
      { dirtyCounts = 0
      , todos = (fmap . fmap) (\todo -> todo { dirty = False }) buffer.todos
      }


----------------------------------------
-- Daemon
----------------------------------------


spawnFlusher :: Handle -> IO ThreadId
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
  { entries       :: [Entry]
  , visibility    :: Text
  , field         :: Text
  , nextId        :: EntryId
  , date          :: Text
  , showCalendar  :: Bool
  , presenceMapL  :: Word32
  , presenceMapR  :: Word32
  }


instance ToJSON Model where
  toJSON model = Aeson.object
    [ "entries"      .= model.entries
    , "visibility"   .= model.visibility
    , "field"        .= model.field
    , "nextId"       .= model.nextId
    , "date"         .= model.date
    , "showCalendar" .= model.showCalendar
    , "presenceMapL" .= model.presenceMapL
    , "presenceMapR" .= model.presenceMapR
    ]


todoToModel :: Todo -> Model
todoToModel todo =
  Model
    { entries      = todo.entries
    , visibility   = "All"
    , field        = mempty
    , nextId       = EntryId (lastId + 1)
    , date         = Text.pack (formatTime defaultTimeLocale  "%Y-%m-%d" todo.date)
    , showCalendar = False
    , presenceMapL = 0
    , presenceMapR = 0
    }
  where
    EntryId lastId
      | null todo.entries = EntryId 0
      | otherwise        = maximum (fmap (.entryId) todo.entries)


json', javascript, css, png, ico, svg :: ByteString -> ActionM ()
json' bytes      = setHeader "Content-Type" "application/json" >> (raw . ByteString.fromStrict $ bytes)
javascript bytes = setHeader "Content-Type" "application/javascript" >> (raw . ByteString.fromStrict $ bytes)
css bytes        = setHeader "Content-Type" "text/css" >> (raw . ByteString.fromStrict $ bytes)
png bytes        = setHeader "Content-Type" "image/png" >> (raw . ByteString.fromStrict $ bytes)
ico bytes        = setHeader "Content-Type" "image/vnd.microsoft.icon" >> (raw . ByteString.fromStrict $ bytes)
svg bytes        = setHeader "Content-Type" "image/svg+xml" >> (raw . ByteString.fromStrict $ bytes)


index :: Model -> Html ()
index model = do
  html_ [ lang_ "en" ] do
    head_ do
      meta_ [ charset_ "UTF-8" ]
      meta_ [ name_ "viewport", content_ "width=device-width, initial-scale=1.0, viewport-fit=cover, maximum-scale=1, user-scalable=no" ]
      meta_ [ httpEquiv_ "X-UA-Compatible", content_ "ie=edge" ]
      meta_ [ name_ "mobile-web-app-capable", content_ "yes" ]
      meta_ [ name_ "apple-mobile-web-app-capable", content_ "yes" ]
      meta_ [ name_ "apple-mobile-web-app-title", content_ "Todou"]
      meta_ [ name_ "apple-mobile-web-app-status-bar-style", content_ "default" ]
      link_ [ rel_ "apple-touch-icon", sizes_ "180x180", href_ "/apple-touch-icon.png"]
      link_ [ rel_ "stylesheet", href_ "/main.css" ]
      link_ [ rel_ "manifest", href_ "/manifest.json" ]
      title_ "Toudo"
    body_ do
      div_ [ id_ "app" ] mempty
      script_ [ id_ "model" ] (Aeson.encode model)
      script_ [ src_ "main.js", type_ "module" ] (mempty @Text)



server :: Options -> Handle -> IO ()
server Options { port } handle = scotty port do
  middleware logStdout


  get "/" do
    mCookies <- header "Cookie"
    let getTimezone = lookup "timezone" . parseCookies . Text.encodeUtf8 . LText.toStrict
    liftIO $ print mCookies
    case mCookies of
      Just cookies
        | Just tz <- getTimezone cookies -> do
            now <- liftIO getCurrentTime
            let timeZone  = fromMaybe utc (readMaybe (Char8.unpack tz))
            let localTime = utcToLocalTime timeZone now
            liftIO $ print timeZone
            redirect ("/" <> LText.pack (formatTime defaultTimeLocale  "%Y-%m-%d" localTime.localDay))
      _ -> do
        html . Lucid.renderText $ do
          html_ do
            head_ do
              script_ do
                [iii| let timezone = new Intl.DateTimeFormat('en-US', { timeZoneName: 'short' })
                        .formatToParts(new Date())
                        .find(part => part.type === 'timeZoneName')
                        .value;
                      document.cookie = "timezone=; path=/; max-age=0";
                      document.cookie = `timezone=${timezone}; path=/; max-age=${7*24*60*60}`;
                      window.location.href = "/"
                    |]


  -- render the todo data for one date.
  get "/:date" do
    date <- captureParam @Day "date"
    presenceMapL <- liftIO $ getPresences handle date [1..30]
    presenceMapR <- liftIO $ getPresences handle date (fmap negate [1..30])
    let bufferMvar = getBufferMVar handle
    buffer <- liftIO do
      flush handle -- flush on refresh
      takeMVar bufferMvar
    case Map.lookup date buffer.todos of
      TodoLoaded todo -> do
        liftIO $ putMVar bufferMvar buffer
        html . Lucid.renderText $ index ((todoToModel todo)
          { presenceMapL = presenceMapL
          , presenceMapR = presenceMapR
          })
      TodoNotLoaded -> do
        liftIO $ putMVar bufferMvar buffer
        liftIO (loadTodo handle date) >>= \case
          Just todo -> html . Lucid.renderText $ index ((todoToModel todo)
            { presenceMapL = presenceMapL
            , presenceMapR = presenceMapR
            })
          Nothing -> do
            status status500
            text "Can't find the todo data"
      TodoNotExists -> do -- not in storage, create an empty one
        let newTodo = Todo { entries = [], date = date, dirty = True }
        liftIO $ putMVar bufferMvar buffer
        html . Lucid.renderText $ index ((todoToModel newTodo)
          { presenceMapL = presenceMapL
          , presenceMapR = presenceMapR
          })


  get "/main.js"                      do javascript $(FileEmbed.embedFile "data/todou/main.js")
  get "/sw.js"                        do javascript $(FileEmbed.embedFile "data/todou/sw.js")
  get "/vdom.js"                      do javascript $(FileEmbed.embedFile "data/todou/vdom.js")
  get "/web-app-manifest-192x192.png" do png        $(FileEmbed.embedFile "data/todou/web-app-manifest-192x192.png")
  get "/web-app-manifest-512x512.png" do png        $(FileEmbed.embedFile "data/todou/web-app-manifest-512x512.png")
  get "/apple-touch-icon.png"         do png        $(FileEmbed.embedFile "data/todou/apple-touch-icon.png")
  get "/favicon.ico"                  do ico        $(FileEmbed.embedFile "data/todou/favicon.ico")
  get "/manifest.json"                do json'      $(FileEmbed.embedFile "data/todou/manifest.json")
  get "/main.css"                     do css        $(FileEmbed.embedFile "data/todou/main.css")
  get "/left-arrow.svg"               do svg        $(FileEmbed.embedFile "data/todou/left-arrow.svg")
  get "/right-arrow.svg"              do svg        $(FileEmbed.embedFile "data/todou/right-arrow.svg")
  get "/x.svg"                        do svg        $(FileEmbed.embedFile "data/todou/x.svg")
  get "/calendar.svg"                 do svg        $(FileEmbed.embedFile "data/todou/calendar.svg")
  get "/favicon.svg"                  do svg        $(FileEmbed.embedFile "data/todou/favicon.svg")


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
        modifyMVar_ (getBufferMVar handle) (pure . updateTodo date (const newTodo))
      Nothing -> do -- create new todo if necessary
        let newTodo = Todo
              { entries = [newEntry]
              , date    = date, dirty = True
              }
        modifyMVar_ (getBufferMVar handle) (pure . insertTodo date newTodo)
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
            modifyMVar_ (getBufferMVar handle)
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
            modifyMVar_ (getBufferMVar handle)
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
        modifyMVar_ (getBufferMVar handle) (pure . updateTodo date (deleteEntry entryId))
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
            modifyMVar_ (getBufferMVar handle)
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
