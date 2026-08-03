-- | The native (pure-Haskell) @pqi@ adapter.
--
-- 'adapter' bundles the three functions that produce a 'Pqi.Connection'
-- whose fields are closures over the underlying native 'Connection.Connection'
-- (which speaks the PostgreSQL wire protocol directly). 'Pqi.Result' and
-- 'Pqi.Cancel' values are constructed the same way, in "Pqi.Native.Types".
module Pqi.Native
  ( adapter,
  )
where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.Map.Strict as Map
import qualified Pqi
import Pqi.Native.Connection (Connection)
import qualified Pqi.Native.Connection as Connection
import qualified Pqi.Native.LargeObject as LargeObject
import Pqi.Native.Prelude
import qualified Pqi.Native.Query as Query
import qualified Pqi.Native.Transport as Transport
import Pqi.Native.Transport.Message
  ( BackendMessage (..),
    copyDataMessage,
    copyDoneMessage,
    copyFailMessage,
    flushMessage,
    syncMessage,
  )
import Pqi.Native.Types (NativeCancel (..), NativeResult (..), mkCancel, mkResult)
import qualified Pqi.Native.UnescapeBytea as UnescapeBytea
import System.Posix.Types (Fd)

-- | The native adapter.
adapter :: Pqi.Adapter
adapter =
  Pqi.Adapter
    { Pqi.name = "pqi-native",
      Pqi.connectdb = \conninfo -> mkConnection <$> Connection.establish conninfo,
      Pqi.connectStart = \conninfo -> mkConnection <$> Connection.establish conninfo,
      Pqi.newNullConnection = mkConnection <$> Connection.nullConnection,
      Pqi.unescapeBytea = \input -> pure (Just (UnescapeBytea.unescapeBytea input))
    }

-- | Build a 'Pqi.Connection' whose fields close over the given native
-- connection.
mkConnection :: Connection -> Pqi.Connection
mkConnection connection =
  Pqi.Connection
    { Pqi.connectPoll = pure Pqi.PollingOk,
      Pqi.isNullConnection = (Connection.isNull connection),
      Pqi.finish = readIORef (Connection.transport connection) >>= Transport.close,
      Pqi.reset = Connection.reconnect connection,
      Pqi.resetStart = Connection.reconnect connection $> True,
      Pqi.resetPoll = pure Pqi.PollingOk,
      Pqi.db = pure (Just (Connection.database (Connection.info connection))),
      Pqi.user = pure (Just (Connection.user (Connection.info connection))),
      Pqi.pass = pure (Just (Connection.password (Connection.info connection))),
      Pqi.host = pure (Just (Connection.host (Connection.info connection))),
      Pqi.port = pure (Just (ByteString.Char8.pack (show (Connection.port (Connection.info connection))))),
      Pqi.options = pure (Just ""),
      Pqi.status = readIORef (Connection.connStatus connection),
      Pqi.transactionStatus = transactionStatusOf <$> readIORef (Connection.txStatus connection),
      Pqi.parameterStatus = \name -> Map.lookup name <$> readIORef (Connection.parameters connection),
      Pqi.protocolVersion = pure 3,
      Pqi.serverVersion =
        maybe 0 parseServerVersion . Map.lookup "server_version" <$> readIORef (Connection.parameters connection),
      Pqi.errorMessage = readIORef (Connection.lastError connection),
      Pqi.socket = do
        transport <- readIORef (Connection.transport connection)
        fd <- Transport.socketFd transport
        pure (Just (fromIntegral fd :: Fd)),
      Pqi.backendPID = maybe 0 fst <$> readIORef (Connection.backendKey connection),
      Pqi.connectionNeedsPassword = pure False,
      Pqi.connectionUsedPassword = pure (not (ByteString.null (Connection.password (Connection.info connection)))),
      Pqi.exec = \sql -> fmap mkResult <$> Query.exec connection sql,
      Pqi.execParams = \sql params resultFormat ->
        fmap mkResult <$> Query.execParams connection sql params resultFormat,
      Pqi.prepare = \name sql parameterTypes ->
        fmap mkResult <$> Query.prepare connection name sql parameterTypes,
      Pqi.execPrepared = \name params resultFormat ->
        fmap mkResult <$> Query.execPrepared connection name params resultFormat,
      Pqi.describePrepared = \name -> fmap mkResult <$> Query.describePrepared connection name,
      Pqi.describePortal = \name -> fmap mkResult <$> Query.describePortal connection name,
      Pqi.escapeStringConn = \value ->
        if isValidUtf8 value
          then pure (Just (ByteString.intercalate "''" (ByteString.split 0x27 value)))
          else pure Nothing,
      Pqi.escapeByteaConn = \value -> pure (Just ("\\x" <> hexEncode value)),
      Pqi.escapeIdentifier = \value ->
        if isValidUtf8 value
          then pure (Just ("\"" <> ByteString.intercalate "\"\"" (ByteString.split 0x22 value) <> "\""))
          else pure Nothing,
      Pqi.sendQuery = Query.sendQuery connection,
      Pqi.sendQueryParams = \sql params resultFormat -> Query.sendQueryParams connection sql params resultFormat,
      Pqi.sendPrepare = \name sql parameterTypes -> Query.sendPrepare connection name sql parameterTypes,
      Pqi.sendQueryPrepared = \name params resultFormat -> Query.sendQueryPrepared connection name params resultFormat,
      Pqi.sendDescribePrepared = Query.sendDescribePrepared connection,
      Pqi.sendDescribePortal = Query.sendDescribePortal connection,
      Pqi.getResult = fmap mkResult <$> Query.getNextResult connection,
      Pqi.consumeInput = pure True,
      Pqi.isBusy = pure False,
      Pqi.setnonblocking = \flag -> writeIORef (Connection.nonblocking connection) flag $> True,
      Pqi.isnonblocking = readIORef (Connection.nonblocking connection),
      Pqi.setSingleRowMode = do
        pending <- readIORef (Connection.asyncPending connection)
        if pending
          then writeIORef (Connection.singleRowMode connection) True $> True
          else pure False,
      Pqi.flush = pure Pqi.FlushOk,
      Pqi.pipelineStatus = readIORef (Connection.pipelineStatus connection),
      Pqi.enterPipelineMode = writeIORef (Connection.pipelineStatus connection) Pqi.PipelineOn $> True,
      Pqi.exitPipelineMode = do
        pending <- readIORef (Connection.asyncPending connection)
        if pending
          then pure False
          else writeIORef (Connection.pipelineStatus connection) Pqi.PipelineOff $> True,
      Pqi.pipelineSync = do
        Connection.sendMessage connection syncMessage
        modifyIORef' (Connection.pendingSyncs connection) (+ 1)
        writeIORef (Connection.asyncPending connection) True
        pure True,
      Pqi.sendFlushRequest = Connection.sendMessage connection flushMessage $> True,
      Pqi.getCancel = do
        key <- readIORef (Connection.backendKey connection)
        pure
          $ fmap
            ( \(pid, secret) ->
                mkCancel
                  NativeCancel
                    { host = Connection.host (Connection.info connection),
                      port = Connection.port (Connection.info connection),
                      pid,
                      secret,
                      asyncPendingRef = (Connection.asyncPending connection),
                      pipelineStatusRef = (Connection.pipelineStatus connection),
                      pendingCommandsRef = (Connection.pendingCommands connection)
                    }
            )
            key,
      Pqi.notifies = popFirst (Connection.pendingNotifications connection),
      Pqi.disableNoticeReporting = writeIORef (Connection.noticeReporting connection) False,
      Pqi.enableNoticeReporting = writeIORef (Connection.noticeReporting connection) True,
      Pqi.getNotice = popFirst (Connection.notices connection),
      Pqi.putCopyData = \payload -> Connection.sendMessage connection (copyDataMessage payload) $> Pqi.CopyInOk,
      Pqi.putCopyEnd = \reason -> do
        Connection.sendMessage connection (maybe copyDoneMessage copyFailMessage reason)
        writeIORef (Connection.asyncPending connection) True
        pure Pqi.CopyInOk,
      Pqi.getCopyData = getCopyData connection,
      Pqi.loCreat = LargeObject.loCreat connection,
      Pqi.loCreate = LargeObject.loCreate connection,
      Pqi.loImport = LargeObject.loImport connection,
      Pqi.loImportWithOid = LargeObject.loImportWithOid connection,
      Pqi.loExport = LargeObject.loExport connection,
      Pqi.loOpen = LargeObject.loOpen connection,
      Pqi.loWrite = LargeObject.loWrite connection,
      Pqi.loRead = LargeObject.loRead connection,
      Pqi.loSeek = LargeObject.loSeek connection,
      Pqi.loTell = LargeObject.loTell connection,
      Pqi.loTruncate = LargeObject.loTruncate connection,
      Pqi.loClose = LargeObject.loClose connection,
      Pqi.loUnlink = LargeObject.loUnlink connection,
      Pqi.clientEncoding =
        fromMaybe "SQL_ASCII" . Map.lookup "client_encoding" <$> readIORef (Connection.parameters connection),
      Pqi.setClientEncoding = \encoding -> do
        result <- Query.exec connection ("SET client_encoding TO '" <> encoding <> "'")
        pure (maybe False (\value -> status value /= Pqi.FatalError) result),
      Pqi.setErrorVerbosity = \verbosity -> do
        previous <- readIORef (Connection.errorVerbosity connection)
        writeIORef (Connection.errorVerbosity connection) verbosity
        pure previous
    }

-- | Receive data on a @COPY TO STDOUT@ connection, as 'Pqi.getCopyData'. The
-- native adapter has no non-blocking transport, so the @Bool@ argument is
-- ignored; it always reads until a full chunk (or the end of the copy) is
-- available.
getCopyData :: Connection -> Bool -> IO Pqi.CopyOutResult
getCopyData connection nonBlocking = do
  message <- Connection.nextMessage connection
  case message of
    CopyData payload -> pure (Pqi.CopyOutRow payload)
    CopyDone -> do
      writeIORef (Connection.asyncPending connection) True
      pure Pqi.CopyOutDone
    CommandComplete _ -> drainToReady connection $> Pqi.CopyOutDone
    ErrorResponse _ -> drainToReady connection $> Pqi.CopyOutError
    ReadyForQuery txState -> writeIORef (Connection.txStatus connection) txState $> Pqi.CopyOutDone
    _ -> getCopyData connection nonBlocking

-- | Read messages until @ReadyForQuery@, recording the transaction status.
drainToReady :: Connection -> IO ()
drainToReady connection = do
  message <- Connection.nextMessage connection
  case message of
    ReadyForQuery txState -> writeIORef (Connection.txStatus connection) txState
    _ -> drainToReady connection

-- | Pop the oldest element of a list stored newest-first.
popFirst :: IORef [a] -> IO (Maybe a)
popFirst ref =
  atomicModifyIORef' ref \xs -> case reverse xs of
    [] -> ([], Nothing)
    oldest : rest -> (reverse rest, Just oldest)

transactionStatusOf :: Word8 -> Pqi.TransactionStatus
transactionStatusOf = \case
  0x49 -> Pqi.TransIdle -- 'I'
  0x54 -> Pqi.TransInTrans -- 'T'
  0x45 -> Pqi.TransInError -- 'E'
  _ -> Pqi.TransUnknown

-- | Parse the @server_version@ parameter into libpq's @MMmmpp@ integer form
-- (e.g. @\"17.2\"@ -> @170002@, @\"9.6.3\"@ -> @90603@).
parseServerVersion :: ByteString -> Int
parseServerVersion raw =
  case ByteString.Char8.readInt raw of
    Nothing -> 0
    Just (major, rest)
      | major >= 10 -> major * 10000 + nextInt rest
      | otherwise ->
          let minor = nextInt rest
              patch = nextInt (dropInt rest)
           in major * 10000 + minor * 100 + patch
  where
    nextInt bs = case ByteString.Char8.uncons bs of
      Just ('.', remainder) -> maybe 0 fst (ByteString.Char8.readInt remainder)
      _ -> 0
    dropInt bs = case ByteString.Char8.uncons bs of
      Just ('.', remainder) -> case ByteString.Char8.readInt remainder of
        Just (_, leftover) -> leftover
        Nothing -> remainder
      _ -> bs

isValidUtf8 :: ByteString -> Bool
isValidUtf8 = go . ByteString.unpack
  where
    go [] = True
    go (b : bs)
      | b < 0x80 = go bs
      | b < 0xc2 = False
      | b < 0xe0 = cont bs 1
      | b < 0xf0 = cont bs 2
      | b < 0xf5 = cont bs 3
      | otherwise = False
    cont bs (0 :: Int) = go bs
    cont [] _ = False
    cont (b : bs) n
      | b .&. 0xc0 == 0x80 = cont bs (n - 1)
      | otherwise = False

hexEncode :: ByteString -> ByteString
hexEncode = ByteString.Char8.pack . concatMap toHex . ByteString.unpack
  where
    toHex byte = [digit (byte `div` 16), digit (byte `mod` 16)]
    digit n
      | n < 10 = toEnum (fromIntegral n + fromEnum '0')
      | otherwise = toEnum (fromIntegral n - 10 + fromEnum 'a')
