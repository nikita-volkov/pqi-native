-- | The native connection: its mutable state, conninfo parsing, the
-- startup\/authentication handshake, and the interleave-aware receive loop that
-- the higher-level query code is built on.
module Pqi.Native.Connection
  ( Connection (..),
    ConnInfo (..),
    parseConnInfo,
    establish,
    nullConnection,
    reconnect,
    nextMessage,
    sendMessage,
    fieldValue,
    setError,
  )
where

import Control.Exception (SomeException, catch)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.Map.Strict as Map
import Pqi (ConnStatus (..), Notify (..), PipelineStatus (..), Verbosity (..))
import qualified Pqi.Native.Auth as Auth
import Pqi.Native.Prelude
import Pqi.Native.Transport (Transport)
import qualified Pqi.Native.Transport as Transport
import Pqi.Native.Transport.Message
import Pqi.Native.Types (formatErrorFields)
import qualified PtrPoker.Write as Poker

-- | Parsed connection parameters (the @key=value@ subset we support).
data ConnInfo = ConnInfo
  { host :: ByteString,
    port :: Int,
    user :: ByteString,
    database :: ByteString,
    password :: ByteString
  }
  deriving stock (Eq, Show)

-- | Parse a @key=value@ conninfo string. Unquoted values only (sufficient for
-- the parameters we handle); URI form, environment variables, and @.pgpass@ are
-- not supported.
parseConnInfo :: ByteString -> ConnInfo
parseConnInfo raw =
  ConnInfo
    { host = get "host" "localhost",
      port = maybe 5432 fst (ByteString.Char8.readInt (get "port" "5432")),
      user = theUser,
      database = get "dbname" theUser,
      password = get "password" ""
    }
  where
    pairs = mapMaybe toPair (ByteString.Char8.words raw)
    settings = Map.fromList pairs
    get key def = Map.findWithDefault def key settings
    theUser = get "user" "postgres"
    toPair token = case ByteString.Char8.break (== '=') token of
      (key, value)
        | not (ByteString.null value) -> Just (key, ByteString.drop 1 value)
      _ -> Nothing

-- | A native connection and its mutable state.
data Connection = Connection
  { transport :: IORef Transport,
    info :: ConnInfo,
    isNull :: Bool,
    parameters :: IORef (Map.Map ByteString ByteString),
    backendKey :: IORef (Maybe (Int32, Int32)),
    txStatus :: IORef Word8,
    connStatus :: IORef ConnStatus,
    lastError :: IORef (Maybe ByteString),
    notices :: IORef [ByteString],
    pendingNotifications :: IORef [Notify],
    noticeReporting :: IORef Bool,
    asyncPending :: IORef Bool,
    nonblocking :: IORef Bool,
    pipelineStatus :: IORef PipelineStatus,
    singleRowMode :: IORef Bool,
    singleRowFields :: IORef [FieldDescription],
    pipelineSeparatorPending :: IORef Bool,
    pendingSyncs :: IORef Int,
    pendingCommands :: IORef Int,
    errorVerbosity :: IORef Verbosity,
    -- | The SQL text of the most recently sent query (set by sendQuery /
    -- sendQueryParams). Used when formatting error messages for async results
    -- so that @LINE N:@ position context can be reproduced.
    currentQuery :: IORef ByteString
  }

-- | Send a serialized frontend message.
sendMessage :: Connection -> Poker.Write -> IO ()
sendMessage connection write = do
  transport <- readIORef connection.transport
  Transport.send transport write

-- | Receive the next /protocol-relevant/ backend message, transparently
-- consuming and recording the asynchronous messages the backend may interleave
-- at any time: @ParameterStatus@ (updates the parameter map), @NoticeResponse@
-- (collected when notice reporting is on), and @NotificationResponse@ (queued).
nextMessage :: Connection -> IO BackendMessage
nextMessage connection = do
  transport <- readIORef connection.transport
  (typeByte, body) <- Transport.receiveFrame transport
  case decodeBackendMessage typeByte body of
    Left err -> ioError (userError ("pqi-native: protocol decode error: " <> show err))
    Right message -> case message of
      ParameterStatus key value -> do
        modifyIORef' connection.parameters (Map.insert key value)
        nextMessage connection
      NoticeResponse fields -> do
        reporting <- readIORef connection.noticeReporting
        when reporting $ do
          let noticeText = formatErrorFields (Map.fromList fields)
          unless (ByteString.null noticeText)
            $ modifyIORef' connection.notices (noticeText :)
        nextMessage connection
      NotificationResponse pid channel payload -> do
        modifyIORef' connection.pendingNotifications (Notify channel pid payload :)
        nextMessage connection
      other -> pure other

-- | Look up an error\/notice field by its single-byte code.
fieldValue :: Word8 -> [(Word8, ByteString)] -> Maybe ByteString
fieldValue code = lookup code

-- | Record a flat error message and mark the connection bad.
setError :: Connection -> ByteString -> IO ()
setError connection message = do
  writeIORef connection.lastError (Just message)
  writeIORef connection.connStatus ConnectionBad

-- | Open a connection: resolve and connect the socket, send the startup
-- message, and run the authentication\/startup handshake. Like libpq, a failed
-- handshake yields a connection in the 'ConnectionBad' state rather than
-- throwing.
establish :: ByteString -> IO Connection
establish conninfo = do
  let info = parseConnInfo conninfo
  transport <- Transport.connect info.host info.port
  connection <- newConnection False transport info
  sendMessage connection (startupMessage [("user", info.user), ("database", info.database)])
  handshake connection
  pure connection

-- | A \"null\" sentinel connection (the analogue of @PQnewNullConnection@): no
-- live socket, permanently in the 'ConnectionBad' state.
nullConnection :: IO Connection
nullConnection = do
  transport <- Transport.unconnected
  conn <- newConnection True transport (parseConnInfo "")
  writeIORef conn.lastError (Just "connection pointer is NULL\n")
  pure conn

-- | Close the current socket and run the startup handshake again on a fresh
-- one, reusing the stored conninfo (the analogue of @PQreset@).
reconnect :: Connection -> IO ()
reconnect connection = do
  oldTransport <- readIORef connection.transport
  Transport.close oldTransport
  newTransport <- Transport.connect connection.info.host connection.info.port
  writeIORef connection.transport newTransport
  writeIORef connection.parameters Map.empty
  writeIORef connection.backendKey Nothing
  writeIORef connection.txStatus 0x49
  writeIORef connection.connStatus ConnectionBad
  writeIORef connection.lastError (Just "")
  sendMessage connection (startupMessage [("user", connection.info.user), ("database", connection.info.database)])
  handshake connection

newConnection :: Bool -> Transport -> ConnInfo -> IO Connection
newConnection isNull transport info = do
  transportRef <- newIORef transport
  Connection transportRef info isNull
    <$> newIORef Map.empty
    <*> newIORef Nothing
    <*> newIORef 0x49 -- 'I'
    <*> newIORef ConnectionBad
    <*> newIORef (Just "")
    <*> newIORef []
    <*> newIORef []
    <*> newIORef False
    <*> newIORef False
    <*> newIORef False
    <*> newIORef PipelineOff
    <*> newIORef False
    <*> newIORef []
    <*> newIORef False
    <*> newIORef 0
    <*> newIORef 0
    <*> newIORef ErrorsDefault
    <*> newIORef ""

-- | The startup\/authentication state machine, ending at the first
-- @ReadyForQuery@ (success) or @ErrorResponse@ (failure).
handshake :: Connection -> IO ()
handshake connection = authenticating
  where
    authenticating = do
      message <- nextMessage connection
      case message of
        AuthenticationOk -> startingUp
        AuthenticationCleartextPassword -> do
          sendMessage connection (passwordMessage connection.info.password)
          authenticating
        AuthenticationMD5Password salt -> do
          let response = Auth.md5Password connection.info.user connection.info.password salt
          sendMessage connection (passwordMessage response)
          authenticating
        AuthenticationSASL mechanisms ->
          Auth.scram connection.info.user connection.info.password mechanisms (saslExchange connection) >>= \case
            Left problem -> setError connection problem
            Right () -> startingUp
        ErrorResponse fields -> failWith fields
        other -> failWith [(0x4d, "unexpected authentication message: " <> ByteString.Char8.pack (show other))]
    startingUp = do
      message <- nextMessage connection
      case message of
        BackendKeyData pid secret -> do
          writeIORef connection.backendKey (Just (pid, secret))
          startingUp
        ReadyForQuery txState -> do
          writeIORef connection.txStatus txState
          writeIORef connection.connStatus ConnectionOk
        ErrorResponse fields -> failWith fields
        _ -> startingUp
    failWith fields = do
      let fmtFields = formatErrorFields (Map.fromList fields)
      transport <- readIORef connection.transport
      mIp <- catch (Just <$> Transport.peerIp transport) (\(_ :: SomeException) -> pure Nothing)
      setError connection $ case mIp of
        Nothing -> fmtFields
        Just ip ->
          "connection to server at \""
            <> connection.info.host
            <> "\" ("
            <> ip
            <> "), port "
            <> ByteString.Char8.pack (show connection.info.port)
            <> " failed: "
            <> fmtFields

-- | The SASL message round-trip used by 'Auth.scram': send a client message and
-- receive the next server SASL\/auth message, projected to the bytes the SCRAM
-- logic needs.
saslExchange :: Connection -> Auth.SaslStep
saslExchange connection =
  Auth.SaslStep
    { Auth.sendInitial = \mechanism initial ->
        sendMessage connection (saslInitialResponse mechanism initial),
      Auth.sendResponse = \payload ->
        sendMessage connection (saslResponse payload),
      Auth.receive = do
        message <- nextMessage connection
        pure $ case message of
          AuthenticationSASLContinue payload -> Auth.SaslContinue payload
          AuthenticationSASLFinal payload -> Auth.SaslFinal payload
          AuthenticationOk -> Auth.SaslOk
          ErrorResponse fields -> Auth.SaslError (fromMaybe "SASL error" (fieldValue 0x4d fields))
          other -> Auth.SaslError ("unexpected SASL message: " <> ByteString.Char8.pack (show other))
    }
