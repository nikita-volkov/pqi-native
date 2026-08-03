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
      Pqi.isNullConnection = connection.isNull,
      Pqi.finish = readIORef connection.transport >>= Transport.close,
      Pqi.reset = Connection.reconnect connection,
      Pqi.resetStart = Connection.reconnect connection $> True,
      Pqi.resetPoll = pure Pqi.PollingOk,
      Pqi.db = pure (Just connection.info.database),
      Pqi.user = pure (Just connection.info.user),
      Pqi.pass = pure (Just connection.info.password),
      Pqi.host = pure (Just connection.info.host),
      Pqi.port = pure (Just (ByteString.Char8.pack (show connection.info.port))),
      Pqi.options = pure (Just ""),
      Pqi.status = readIORef connection.connStatus,
      Pqi.transactionStatus = transactionStatusOf <$> readIORef connection.txStatus,
      Pqi.parameterStatus = \name -> Map.lookup name <$> readIORef connection.parameters,
      Pqi.protocolVersion = pure 3,
      Pqi.serverVersion =
        maybe 0 parseServerVersion . Map.lookup "server_version" <$> readIORef connection.parameters,
      Pqi.errorMessage = readIORef connection.lastError,
      Pqi.socket = do
        transport <- readIORef connection.transport
        fd <- Transport.socketFd transport
        pure (Just (fromIntegral fd :: Fd)),
      Pqi.backendPID = maybe 0 fst <$> readIORef connection.backendKey,
      Pqi.connectionNeedsPassword = pure False,
      Pqi.connectionUsedPassword = pure (not (ByteString.null connection.info.password)),
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
      Pqi.setnonblocking = \flag -> writeIORef connection.nonblocking flag $> True,
      Pqi.isnonblocking = readIORef connection.nonblocking,
      Pqi.setSingleRowMode = do
        pending <- readIORef connection.asyncPending
        if pending
          then writeIORef connection.singleRowMode True $> True
          else pure False,
      Pqi.flush = pure Pqi.FlushOk,
      Pqi.pipelineStatus = readIORef connection.pipelineStatus,
      Pqi.enterPipelineMode = writeIORef connection.pipelineStatus Pqi.PipelineOn $> True,
      Pqi.exitPipelineMode = do
        pending <- readIORef connection.asyncPending
        if pending
          then pure False
          else writeIORef connection.pipelineStatus Pqi.PipelineOff $> True,
      Pqi.pipelineSync = do
        Connection.sendMessage connection syncMessage
        modifyIORef' connection.pendingSyncs (+ 1)
        writeIORef connection.asyncPending True
        pure True,
      Pqi.sendFlushRequest = Connection.sendMessage connection flushMessage $> True,
      Pqi.getCancel = do
        key <- readIORef connection.backendKey
        pure
          $ fmap
            ( \(pid, secret) ->
                mkCancel
                  NativeCancel
                    { host = connection.info.host,
                      port = connection.info.port,
                      pid,
                      secret,
                      asyncPendingRef = connection.asyncPending,
                      pipelineStatusRef = connection.pipelineStatus,
                      pendingCommandsRef = connection.pendingCommands
                    }
            )
            key,
      Pqi.notifies = popFirst connection.pendingNotifications,
      Pqi.disableNoticeReporting = writeIORef connection.noticeReporting False,
      Pqi.enableNoticeReporting = writeIORef connection.noticeReporting True,
      Pqi.getNotice = popFirst connection.notices,
      Pqi.putCopyData = \payload -> Connection.sendMessage connection (copyDataMessage payload) $> Pqi.CopyInOk,
      Pqi.putCopyEnd = \reason -> do
        Connection.sendMessage connection (maybe copyDoneMessage copyFailMessage reason)
        writeIORef connection.asyncPending True
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
        fromMaybe "SQL_ASCII" . Map.lookup "client_encoding" <$> readIORef connection.parameters,
      Pqi.setClientEncoding = \encoding -> do
        result <- Query.exec connection ("SET client_encoding TO '" <> encoding <> "'")
        pure (maybe False (\value -> value.status /= Pqi.FatalError) result),
      Pqi.setErrorVerbosity = \verbosity -> do
        previous <- readIORef connection.errorVerbosity
        writeIORef connection.errorVerbosity verbosity
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
      writeIORef connection.asyncPending True
      pure Pqi.CopyOutDone
    CommandComplete _ -> drainToReady connection $> Pqi.CopyOutDone
    ErrorResponse _ -> drainToReady connection $> Pqi.CopyOutError
    ReadyForQuery txState -> writeIORef connection.txStatus txState $> Pqi.CopyOutDone
    _ -> getCopyData connection nonBlocking

-- | Read messages until @ReadyForQuery@, recording the transaction status.
drainToReady :: Connection -> IO ()
drainToReady connection = do
  message <- Connection.nextMessage connection
  case message of
    ReadyForQuery txState -> writeIORef connection.txStatus txState
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
