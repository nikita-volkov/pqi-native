{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | The native (pure-Haskell) @pqi@ adapter.
--
-- 'Connection' speaks the PostgreSQL wire protocol directly.
-- Provides the 'IsConnection' instance.
module Pqi.Native
  ( Connection,
  )
where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.Map.Strict as Map
import Pqi
import Pqi.Native.Connection (Connection (..))
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
import Pqi.Native.Types (NativeCancel (..), NativeResult (..))
import System.Posix.Types (Fd)

instance IsConnection Connection where
  type ResultOf Connection = NativeResult
  type CancelOf Connection = NativeCancel

  connectdb = Connection.establish
  connectStart = Connection.establish
  connectPoll _ = pure PollingOk
  newNullConnection = Connection.nullConnection
  isNullConnection connection = connection.isNull
  finish connection = readIORef connection.transport >>= Transport.close
  reset = Connection.reconnect
  resetStart connection = Connection.reconnect connection $> True
  resetPoll _ = pure PollingOk
  db connection = pure (Just connection.info.database)
  user connection = pure (Just connection.info.user)
  pass connection = pure (Just connection.info.password)
  host connection = pure (Just connection.info.host)
  port connection = pure (Just (ByteString.Char8.pack (show connection.info.port)))
  options _ = pure (Just "")
  status connection = readIORef connection.connStatus
  transactionStatus connection = transactionStatusOf <$> readIORef connection.txStatus
  parameterStatus connection name = Map.lookup name <$> readIORef connection.parameters
  protocolVersion _ = pure 3
  serverVersion connection =
    maybe 0 parseServerVersion . Map.lookup "server_version" <$> readIORef connection.parameters
  errorMessage connection = readIORef connection.lastError
  socket connection = do
    transport <- readIORef connection.transport
    fd <- Transport.socketFd transport
    pure (Just (fromIntegral fd :: Fd))
  backendPID connection = maybe 0 fst <$> readIORef connection.backendKey
  connectionNeedsPassword _ = pure False
  connectionUsedPassword connection = pure (not (ByteString.null connection.info.password))

  exec connection sql = Query.exec connection sql
  execParams connection sql params resultFormat =
    Query.execParams connection sql params resultFormat
  prepare connection name sql parameterTypes =
    Query.prepare connection name sql parameterTypes
  execPrepared connection name params resultFormat =
    Query.execPrepared connection name params resultFormat
  describePrepared connection name = Query.describePrepared connection name
  describePortal connection name = Query.describePortal connection name

  escapeStringConn _ value
    | isValidUtf8 value =
        pure (Just (ByteString.intercalate "''" (ByteString.split 0x27 value)))
    | otherwise = pure Nothing
  escapeByteaConn _ value =
    pure (Just ("\\x" <> hexEncode value))
  escapeIdentifier _ value
    | isValidUtf8 value =
        pure (Just ("\"" <> ByteString.intercalate "\"\"" (ByteString.split 0x22 value) <> "\""))
    | otherwise = pure Nothing

  sendQuery = Query.sendQuery
  sendQueryParams = Query.sendQueryParams
  sendPrepare = Query.sendPrepare
  sendQueryPrepared = Query.sendQueryPrepared
  sendDescribePrepared = Query.sendDescribePrepared
  sendDescribePortal = Query.sendDescribePortal
  getResult connection = Query.getNextResult connection
  consumeInput _ = pure True
  isBusy _ = pure False
  setnonblocking connection flag = writeIORef connection.nonblocking flag $> True
  isnonblocking connection = readIORef connection.nonblocking
  setSingleRowMode connection = do
    pending <- readIORef connection.asyncPending
    if pending
      then writeIORef connection.singleRowMode True $> True
      else pure False
  flush _ = pure FlushOk

  pipelineStatus connection = readIORef connection.pipelineStatus
  enterPipelineMode connection = writeIORef connection.pipelineStatus PipelineOn $> True
  exitPipelineMode connection = do
    pending <- readIORef connection.asyncPending
    if pending
      then pure False
      else writeIORef connection.pipelineStatus PipelineOff $> True
  pipelineSync connection = do
    Connection.sendMessage connection syncMessage
    modifyIORef' connection.pendingSyncs (+ 1)
    pure True
  sendFlushRequest connection = Connection.sendMessage connection flushMessage $> True

  getCancel connection = do
    key <- readIORef connection.backendKey
    pure (fmap (\(pid, secret) -> NativeCancel connection.info.host connection.info.port pid secret) key)

  notifies connection = popFirst connection.pendingNotifications
  disableNoticeReporting connection = writeIORef connection.noticeReporting False
  enableNoticeReporting connection = writeIORef connection.noticeReporting True
  getNotice connection = popFirst connection.notices

  putCopyData connection payload = Connection.sendMessage connection (copyDataMessage payload) $> CopyInOk
  putCopyEnd connection reason = do
    Connection.sendMessage connection (maybe copyDoneMessage copyFailMessage reason)
    writeIORef connection.asyncPending True
    pure CopyInOk
  getCopyData connection _nonBlocking = do
    message <- Connection.nextMessage connection
    case message of
      CopyData payload -> pure (CopyOutRow payload)
      CopyDone -> do
        writeIORef connection.asyncPending True
        pure CopyOutDone
      CommandComplete _ -> drainToReady connection $> CopyOutDone
      ErrorResponse _ -> drainToReady connection $> CopyOutError
      ReadyForQuery txState -> writeIORef connection.txStatus txState $> CopyOutDone
      _ -> getCopyData connection _nonBlocking

  loCreat = LargeObject.loCreat
  loCreate = LargeObject.loCreate
  loImport = LargeObject.loImport
  loImportWithOid = LargeObject.loImportWithOid
  loExport = LargeObject.loExport
  loOpen = LargeObject.loOpen
  loWrite = LargeObject.loWrite
  loRead = LargeObject.loRead
  loSeek = LargeObject.loSeek
  loTell = LargeObject.loTell
  loTruncate = LargeObject.loTruncate
  loClose = LargeObject.loClose
  loUnlink = LargeObject.loUnlink

  clientEncoding connection =
    fromMaybe "SQL_ASCII" . Map.lookup "client_encoding" <$> readIORef connection.parameters
  setClientEncoding connection encoding = do
    result <- Query.exec connection ("SET client_encoding TO '" <> encoding <> "'")
    pure (maybe False (\value -> value.status /= FatalError) result)
  setErrorVerbosity connection verbosity = do
    previous <- readIORef connection.errorVerbosity
    writeIORef connection.errorVerbosity verbosity
    pure previous

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

transactionStatusOf :: Word8 -> TransactionStatus
transactionStatusOf = \case
  0x49 -> TransIdle -- 'I'
  0x54 -> TransInTrans -- 'T'
  0x45 -> TransInError -- 'E'
  _ -> TransUnknown

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
