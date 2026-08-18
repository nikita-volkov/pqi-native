{-# LANGUAGE CPP #-}

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

import Control.Exception (IOException, SomeException, catch, try)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import GHC.IO.Exception (ioe_description)
import Pqi (ConnStatus (..), Notify (..), PipelineStatus (..), Verbosity (..))
import qualified Pqi.Native.Auth as Auth
import Pqi.Native.Prelude
import Pqi.Native.Transport (Transport)
import qualified Pqi.Native.Transport as Transport
import Pqi.Native.Transport.Message
import Pqi.Native.Types (formatErrorFields)
import qualified PtrPoker.Write as Poker
import System.Environment (lookupEnv)
import System.IO.Error (isEOFError)
#if defined(mingw32_HOST_OS)
import System.Win32.Info.Computer (getUserName)
#else
import System.Posix.User (getEffectiveUserName)
#endif

-- | Parsed connection parameters (the @key=value@ subset we support).
data ConnInfo = ConnInfo
  { host :: ByteString,
    port :: Int,
    user :: ByteString,
    database :: ByteString,
    password :: ByteString,
    -- | Every other recognized @key=value@ pair (e.g. @application_name@,
    -- @options@), forwarded verbatim in the startup message so the server
    -- sees them, the way libpq does.
    extraParams :: Map.Map ByteString ByteString
  }
  deriving stock (Eq, Show)

-- | Parse a conninfo string in either @key=value@ or @postgresql:\/\/@ URI
-- format. Unquoted key=value values only; URI values are percent-decoded.
-- @.pgpass@ is not supported.
--
-- The @dfltUser@\/@dfltHost@ arguments are the already-resolved defaults (see
-- 'resolveDefaultUser'\/'resolveDefaultHost'), used whenever the conninfo
-- omits @user@\/@host@ respectively. They are passed in explicitly because
-- resolving them is the only IO this otherwise-pure parser needs.
parseConnInfo :: ByteString -> ByteString -> ByteString -> ConnInfo
parseConnInfo raw dfltUser dfltHost =
  if
    | "postgresql://" `ByteString.isPrefixOf` raw -> parseUri dfltUser dfltHost (ByteString.drop 13 raw)
    | "postgres://" `ByteString.isPrefixOf` raw -> parseUri dfltUser dfltHost (ByteString.drop 11 raw)
    | otherwise -> parseKeyValue dfltUser dfltHost raw

-- | Resolve the default @user@ the way libpq does (@conninfo_add_defaults@ /
-- @pg_fe_getauthname@ in @fe-connect.c@): the @PGUSER@ environment variable if
-- set and non-empty, otherwise the operating-system login name
-- (@getpwuid(geteuid())->pw_name@ on Unix, @GetUserName@ on Windows).
--
-- Returns @Left msg@ if neither is available. Mirroring libpq, a lookup failure
-- must be surfaced by the caller as a 'ConnectionBad' connection (see
-- 'establish') rather than attempting to connect.
resolveDefaultUser :: IO (Either String ByteString)
resolveDefaultUser = do
  pguser <- lookupEnv "PGUSER"
  case pguser of
    Just u | not (null u) -> pure (Right (ByteString.Char8.pack u))
    _ -> do
      result <- try @SomeException (ByteString.Char8.pack <$> platformUserName)
      pure case result of
        Right name -> Right name
        Left err -> Left (platformUserNameLookupFailureMessage <> ": " <> show err)

-- | The operating-system login name, via the same call libpq uses:
-- 'getEffectiveUserName' (@getpwuid(geteuid())@) on Unix, 'getUserName'
-- (@GetUserName@) on Windows.
platformUserName :: IO String
#if defined(mingw32_HOST_OS)
platformUserName = getUserName
#else
platformUserName = getEffectiveUserName
#endif

#if defined(mingw32_HOST_OS)
platformUserNameLookupFailureMessage :: String
platformUserNameLookupFailureMessage = "user name lookup failure"
#else
platformUserNameLookupFailureMessage :: String
platformUserNameLookupFailureMessage = "could not look up local user name"
#endif

-- | The default @host@ used when a conninfo omits it (or gives an empty
-- value), matching libpq's own @conninfo_add_defaults@ / @PQconnectdbParams@
-- resolution: @PGHOST@ if set and non-empty, otherwise 'defaultUnixSocketDir'
-- (or @localhost@ on Windows, where there's no Unix-domain default to fall
-- back to).
--
-- An explicit @host=\/some\/path@ (or its URI equivalent) always connects via
-- Unix-domain socket regardless of this default - see
-- 'Transport.isUnixSocketHost'.
resolveDefaultHost :: IO ByteString
resolveDefaultHost = do
  pghost <- lookupEnv "PGHOST"
  pure $ case pghost of
    Just h | not (null h) -> ByteString.Char8.pack h
    _ -> compiledDefaultHost

compiledDefaultHost :: ByteString
#if defined(mingw32_HOST_OS)
compiledDefaultHost = "localhost"
#else
compiledDefaultHost = defaultUnixSocketDir
#endif

#ifndef PQI_NATIVE_DEFAULT_UNIX_SOCKET_DIR
#define PQI_NATIVE_DEFAULT_UNIX_SOCKET_DIR "/tmp"
#endif

-- | The Unix-domain socket directory used when neither @host@ nor @PGHOST@ is
-- given (non-Windows only). Defaults to @\/tmp@, the directory the upstream
-- @postgres.org@ @libpq@ uses. A distribution that compiles its own @libpq@
-- with a different default can match it here via @cabal.project@. Note this
-- needs the whole option double-quoted with the inner quotes backslash-escaped
-- (@cabal.project@'s per-package field parser tokenizes @ghc-options@
-- shell-style).
--
-- > package pqi-native
-- >   ghc-options: "-DPQI_NATIVE_DEFAULT_UNIX_SOCKET_DIR=\"/run/postgresql\""
#if !defined(mingw32_HOST_OS)
defaultUnixSocketDir :: ByteString
defaultUnixSocketDir = PQI_NATIVE_DEFAULT_UNIX_SOCKET_DIR
#endif

parseKeyValue :: ByteString -> ByteString -> ByteString -> ConnInfo
parseKeyValue dfltUser dfltHost raw =
  ConnInfo
    { host = defaultIfEmpty dfltHost (get "host" ""),
      port = maybe 5432 fst (ByteString.Char8.readInt (get "port" "5432")),
      user = theUser,
      database = get "dbname" theUser,
      password = get "password" "",
      extraParams = Map.withoutKeys settings reservedKeys
    }
  where
    pairs = mapMaybe toPair (ByteString.Char8.words raw)
    settings = Map.fromList pairs
    get key def = Map.findWithDefault def key settings
    theUser = get "user" dfltUser
    toPair token = case ByteString.Char8.break (== '=') token of
      (key, value)
        | not (ByteString.null value) -> Just (key, ByteString.drop 1 value)
      _ -> Nothing

defaultIfEmpty :: ByteString -> ByteString -> ByteString
defaultIfEmpty dflt raw
  | ByteString.null raw = dflt
  | otherwise = raw

-- | Conninfo keys already surfaced via their own 'ConnInfo' fields, so they're
-- excluded from 'extraParams' rather than duplicated there.
reservedKeys :: Set.Set ByteString
reservedKeys = Set.fromList ["host", "port", "user", "dbname", "password"]

-- | Parse the authority+path portion of a @postgresql://@ URI (scheme already
-- stripped). Handles @[user[:password]@][host[:port]][/dbname]@; ignores query
-- parameters other than what appears in those components.
parseUri :: ByteString -> ByteString -> ByteString -> ConnInfo
parseUri dfltUser dfltHost withoutScheme =
  ConnInfo {host, port, user, database, password, extraParams}
  where
    -- Split off optional "userinfo@" prefix. The '@' is unambiguous in this
    -- position: hosts do not contain '@' in practice.
    (userinfoMay, afterAt) = case ByteString.elemIndex 0x40 withoutScheme of
      Just i -> (Just (ByteString.take i withoutScheme), ByteString.drop (i + 1) withoutScheme)
      Nothing -> (Nothing, withoutScheme)

    (user, password) = case userinfoMay of
      Nothing -> (dfltUser, "")
      Just ui -> case ByteString.elemIndex 0x3a ui of
        Just c -> (pctDecode (ByteString.take c ui), pctDecode (ByteString.drop (c + 1) ui))
        Nothing -> (pctDecode ui, "")

    -- Split host[:port] from /dbname?params.
    (hostport, pathAndQuery) = ByteString.break (== 0x2f) afterAt

    -- Strip optional leading '/' to get just the dbname (ignoring ?query).
    rawDbname =
      let p = ByteString.drop 1 pathAndQuery
       in ByteString.takeWhile (/= 0x3f) p

    database =
      if ByteString.null rawDbname
        then user
        else pctDecode rawDbname

    -- Everything after the first '?', parsed as '&'-separated key=value pairs
    -- (e.g. ?application_name=foo&sslmode=disable).
    rawQuery = ByteString.drop 1 (ByteString.dropWhile (/= 0x3f) pathAndQuery)

    extraParams =
      Map.withoutKeys
        (Map.fromList (mapMaybe toQueryPair (ByteString.split 0x26 rawQuery)))
        reservedKeys

    toQueryPair token = case ByteString.elemIndex 0x3d token of
      Just i ->
        let (k, v) = ByteString.splitAt i token
         in if ByteString.null k then Nothing else Just (pctDecode k, pctDecode (ByteString.drop 1 v))
      Nothing -> Nothing

    -- Parse host and port from "host:port", handling IPv6 "[::1]:port".
    (host, port)
      | not (ByteString.null hostport) && ByteString.head hostport == 0x5b =
          case ByteString.elemIndex 0x5d hostport of
            Just close ->
              let h = ByteString.take close (ByteString.drop 1 hostport)
                  portStr = ByteString.drop 2 (ByteString.drop close hostport)
               in (pctDecode h, readPort portStr)
            Nothing -> (pctDecode hostport, 5432)
      | otherwise = case ByteString.elemIndexEnd 0x3a hostport of
          Just c ->
            let h = ByteString.take c hostport
                p = ByteString.drop (c + 1) hostport
             in if ByteString.null h then (dfltHost, readPort p) else (pctDecode h, readPort p)
          Nothing ->
            (defaultIfEmpty dfltHost (pctDecode hostport), 5432)

    readPort bs = maybe 5432 fst (ByteString.Char8.readInt bs)

-- | Decode @%XX@ percent-encoding in a URI component.
pctDecode :: ByteString -> ByteString
pctDecode bs
  | ByteString.null bs = ByteString.empty
  | ByteString.head bs == 0x25,
    ByteString.length bs >= 3,
    Just hi <- hexVal (ByteString.index bs 1),
    Just lo <- hexVal (ByteString.index bs 2) =
      ByteString.singleton (fromIntegral (hi * 16 + lo)) <> pctDecode (ByteString.drop 3 bs)
  | otherwise = ByteString.singleton (ByteString.head bs) <> pctDecode (ByteString.tail bs)
  where
    hexVal w
      | w >= 0x30 && w <= 0x39 = Just (fromIntegral w - 0x30 :: Int)
      | w >= 0x41 && w <= 0x46 = Just (fromIntegral w - 0x41 + 10 :: Int)
      | w >= 0x61 && w <= 0x66 = Just (fromIntegral w - 0x61 + 10 :: Int)
      | otherwise = Nothing

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
    -- | FIFO, one entry per in-flight pipelined command, pushed by every
    -- 'sendAsync' call while in pipeline mode and popped exactly once by
    -- whichever message ends that command's processing. For a command that
    -- sends a @Parse@ ('sendQueryParams' or 'sendPrepare') the entry records
    -- whether that command's @ParseComplete@ is itself the terminal result:
    -- 'sendPrepare' pushes @Just True@ (its @ParseComplete@ ends the command
    -- as 'CommandOk'); 'sendQueryParams' pushes @Just False@ (its
    -- @ParseComplete@ must fold into the command's accumulating result, like
    -- every other extended-protocol message, and never terminate it early).
    -- A command with no @Parse@ step (e.g. 'sendQueryPrepared') pushes
    -- @Nothing@, popped (and discarded) at its own terminal message. Popping
    -- the head at the right message keeps the origins in order even when
    -- several commands are pipelined together - a plain counter cannot,
    -- which is what let a pipelined 'sendPrepare' steal the 'ParseComplete'
    -- of an earlier 'sendQueryParams' and shift every later result by one.
    --
    -- Every command pushes exactly one entry and must pop exactly one, even
    -- when it fails before reaching @ParseComplete@ (e.g. a syntax error, or
    -- being discarded after a pipeline abort): leaving that entry unpopped
    -- would let a later, unrelated command's @ParseComplete@ pop it instead,
    -- misattributing that later command's origin and - when the leaked entry
    -- happens to be @Just True@ - making it terminate early as 'CommandOk'
    -- instead of collecting its actual result (e.g. 'TuplesOk' for a
    -- @SELECT@).
    pendingParseOrigins :: IORef (Seq.Seq (Maybe Bool)),
    errorVerbosity :: IORef Verbosity,
    -- | The SQL text of the most recently sent query (set by sendQuery /
    -- sendQueryParams). Used when formatting error messages for async results
    -- so that @LINE N:@ position context can be reproduced.
    currentQuery :: IORef ByteString
  }

-- | Send a serialized frontend message.
sendMessage :: Connection -> Poker.Write -> IO ()
sendMessage connection write = do
  transport <- readIORef (transport connection)
  Transport.send transport write

-- | Receive the next /protocol-relevant/ backend message, transparently
-- consuming and recording the asynchronous messages the backend may interleave
-- at any time: @ParameterStatus@ (updates the parameter map), @NoticeResponse@
-- (collected when notice reporting is on), and @NotificationResponse@ (queued).
nextMessage :: Connection -> IO BackendMessage
nextMessage connection = do
  transport <- readIORef (transport connection)
  (typeByte, body) <- Transport.receiveFrame transport
  case decodeBackendMessage typeByte body of
    Left err -> ioError (userError ("pqi-native: protocol decode error: " <> show err))
    Right message -> case message of
      ParameterStatus key value -> do
        modifyIORef' (parameters connection) (Map.insert key value)
        nextMessage connection
      NoticeResponse fields -> do
        reporting <- readIORef (noticeReporting connection)
        when reporting $ do
          let noticeText = formatErrorFields (Map.fromList fields)
          unless (ByteString.null noticeText)
            $ modifyIORef' (notices connection) (noticeText :)
        nextMessage connection
      NotificationResponse pid channel payload -> do
        modifyIORef' (pendingNotifications connection) (Notify channel pid payload :)
        nextMessage connection
      other -> pure other

-- | Look up an error\/notice field by its single-byte code.
fieldValue :: Word8 -> [(Word8, ByteString)] -> Maybe ByteString
fieldValue code = lookup code

-- | Record a flat error message and mark the connection bad.
setError :: Connection -> ByteString -> IO ()
setError connection message = do
  writeIORef (lastError connection) (Just message)
  writeIORef (connStatus connection) ConnectionBad

-- | Format an initial-connect failure (the socket couldn't even be opened),
-- matching libpq's distinct phrasing for a Unix-domain socket vs. a TCP host.
connectFailureMessage :: ConnInfo -> IOException -> ByteString
connectFailureMessage connInfo err
  | Transport.isUnixSocketHost (host connInfo) =
      unixSocketFailureMessage
        connInfo
        ( ByteString.Char8.pack (ioe_description err)
            <> "\n\tIs the server running locally and accepting connections on that socket?\n"
        )
  | otherwise = "could not connect to server: " <> ByteString.Char8.pack (show err)

-- | Format a handshake-time 'IOException' - e.g. the server closing the
-- socket mid-rejection, as when shedding load with \"sorry, too many clients
-- already\": the rejection is sent but the socket closes before 'handshake'
-- finishes reading it. Routed through the same 'unixSocketFailureMessage'\/
-- 'tcpFailureMessage' wrapper 'failWith' uses for a rejected 'ErrorResponse',
-- so a failure that interrupts the handshake reads exactly like any other
-- classified rejection instead of escaping 'establish' as an uncaught
-- exception. An EOF (the frame never completing) gets libpq's own wording for
-- it; any other handshake-time I\/O error falls back to its 'show'n form.
handshakeFailureMessage :: Connection -> ConnInfo -> IOException -> IO ByteString
handshakeFailureMessage connection connInfo err = do
  let fmtFields
        | isEOFError err =
            "server closed the connection unexpectedly\n\tThis probably means the server terminated abnormally\n\tbefore or while processing the request.\n"
        | otherwise = ByteString.Char8.pack (show err)
  if Transport.isUnixSocketHost (host connInfo)
    then pure (unixSocketFailureMessage connInfo fmtFields)
    else tcpFailureMessage connection connInfo fmtFields

-- | The handshake-failure message ('failWith', inside 'handshake') for a
-- Unix-domain socket connection: names the socket path rather than a
-- host\/port pair, matching libpq's phrasing.
unixSocketFailureMessage :: ConnInfo -> ByteString -> ByteString
unixSocketFailureMessage connInfo fmtFields =
  "connection to server on socket \""
    <> ByteString.Char8.pack (Transport.unixSocketPath (host connInfo) (port connInfo))
    <> "\" failed: "
    <> fmtFields

-- | The handshake-failure message for a TCP connection: includes the
-- resolved peer IP when available and distinct from the given host (it may
-- not be resolvable at all, e.g. if the socket has already been torn down;
-- and libpq omits the parenthetical entirely when the host was already the
-- literal numeric address, rather than a name that resolved to it), matching
-- libpq's phrasing.
tcpFailureMessage :: Connection -> ConnInfo -> ByteString -> IO ByteString
tcpFailureMessage connection connInfo fmtFields = do
  transport <- readIORef (transport connection)
  mIp <- catch (Just <$> Transport.peerIp transport) (\(_ :: SomeException) -> pure Nothing)
  pure $ case mIp of
    Just ip
      | ip /= host connInfo ->
          "connection to server at \""
            <> host connInfo
            <> "\" ("
            <> ip
            <> "), port "
            <> ByteString.Char8.pack (show (port connInfo))
            <> " failed: "
            <> fmtFields
    _ ->
      "connection to server at \""
        <> host connInfo
        <> "\", port "
        <> ByteString.Char8.pack (show (port connInfo))
        <> " failed: "
        <> fmtFields

-- | Open a connection: resolve and connect the socket, send the startup
-- message, and run the authentication\/startup handshake. Like libpq, a failed
-- connection (whether due to a network error or a rejected handshake) yields a
-- 'ConnectionBad' connection rather than throwing.
-- | Open a connection: resolve the default user, resolve and connect the
-- socket, send the startup message, and run the authentication\/startup
-- handshake. Like libpq, a failed connection - whether due to a user-name
-- lookup failure, a network error or a rejected handshake - yields a
-- 'ConnectionBad' connection rather than throwing.
establish :: ByteString -> IO Connection
establish conninfo = do
  dfltHost <- resolveDefaultHost
  userResult <- resolveDefaultUser
  case userResult of
    Left message -> do
      transport <- Transport.unconnected
      connection <- newConnection False transport (parseConnInfo conninfo "" dfltHost)
      setError connection (ByteString.Char8.pack message)
      pure connection
    Right dfltUser -> do
      let info = parseConnInfo conninfo dfltUser dfltHost
      transportResult <- try @IOException (Transport.connect (host info) (port info))
      case transportResult of
        Left err -> do
          transport <- Transport.unconnected
          connection <- newConnection False transport info
          setError connection (connectFailureMessage info err)
          pure connection
        Right transport -> do
          connection <- newConnection False transport info
          sendMessage connection (startupMessage (startupParams info))
          handshakeResult <- try @IOException (handshake connection)
          case handshakeResult of
            Left err -> setError connection =<< handshakeFailureMessage connection info err
            Right () -> pure ()
          pure connection

-- | A \"null\" sentinel connection (the analogue of @PQnewNullConnection@): no
-- live socket, permanently in the 'ConnectionBad' state.
nullConnection :: IO Connection
nullConnection = do
  transport <- Transport.unconnected
  let info = parseConnInfo "" "" "localhost"
  conn <- newConnection True transport info
  writeIORef (lastError conn) (Just "connection pointer is NULL\n")
  pure conn

-- | Close the current socket and run the startup handshake again on a fresh
-- one, reusing the stored conninfo (the analogue of @PQreset@).
reconnect :: Connection -> IO ()
reconnect connection = do
  oldTransport <- readIORef (transport connection)
  Transport.close oldTransport
  newTransport <- Transport.connect (host (info connection)) (port (info connection))
  writeIORef (transport connection) newTransport
  writeIORef (parameters connection) Map.empty
  writeIORef (backendKey connection) Nothing
  writeIORef (txStatus connection) 0x49
  writeIORef (connStatus connection) ConnectionBad
  writeIORef (lastError connection) (Just "")
  sendMessage connection (startupMessage (startupParams (info connection)))
  handshake connection

-- | The startup message parameter list: @user@ and @database@, plus any
-- extra conninfo params (e.g. @application_name@) forwarded verbatim.
startupParams :: ConnInfo -> [(ByteString, ByteString)]
startupParams info =
  ("user", user info) : ("database", database info) : Map.toList (extraParams info)

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
    <*> newIORef Seq.empty
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
          sendMessage connection (passwordMessage (password (info connection)))
          authenticating
        AuthenticationMD5Password salt -> do
          let response = Auth.md5Password (user (info connection)) (password (info connection)) salt
          sendMessage connection (passwordMessage response)
          authenticating
        AuthenticationSASL mechanisms ->
          Auth.scram (user (info connection)) (password (info connection)) mechanisms (saslExchange connection) >>= \case
            Left problem -> setError connection problem
            Right () -> startingUp
        ErrorResponse fields -> failWith fields
        other -> failWith [(0x4d, "unexpected authentication message: " <> ByteString.Char8.pack (show other))]
    startingUp = do
      message <- nextMessage connection
      case message of
        BackendKeyData pid secret -> do
          writeIORef (backendKey connection) (Just (pid, secret))
          startingUp
        ReadyForQuery txState -> do
          writeIORef (txStatus connection) txState
          writeIORef (connStatus connection) ConnectionOk
        ErrorResponse fields -> failWith fields
        _ -> startingUp
    failWith fields = do
      let fmtFields = formatErrorFields (Map.fromList fields)
          connInfo = info connection
      message <-
        if Transport.isUnixSocketHost (host connInfo)
          then pure (unixSocketFailureMessage connInfo fmtFields)
          else tcpFailureMessage connection connInfo fmtFields
      setError connection message

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
