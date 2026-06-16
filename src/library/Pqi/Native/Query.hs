-- | Command execution: the simple- and extended-query flows, and the
-- materialization of the backend message stream into a 'NativeResult'.
module Pqi.Native.Query
  ( exec,
    execParams,
    prepare,
    execPrepared,
    describePrepared,
    describePortal,
    sendQuery,
    sendQueryParams,
    sendPrepare,
    sendQueryPrepared,
    sendDescribePrepared,
    sendDescribePortal,
    getNextResult,
  )
where

import qualified Data.Map.Strict as Map
import Pqi (ConnStatus (..), ExecStatus (..), Format (..), PipelineStatus (..))
import Pqi.Native.Connection
import Pqi.Native.Prelude
import Pqi.Native.Transport.Message
import Pqi.Native.Types (NativeResult (..), formatResultError)
import qualified PtrPoker.Write as Poker

-- * Message construction

-- Sync-inclusive variants used by the synchronous exec* functions.

paramsWrite :: ByteString -> [Maybe (Word32, ByteString, Format)] -> Format -> Poker.Write
paramsWrite sql params resultFormat =
  asyncParamsWrite sql params resultFormat <> syncMessage

preparedWrite :: ByteString -> [Maybe (ByteString, Format)] -> Format -> Poker.Write
preparedWrite name params resultFormat =
  asyncPreparedWrite name params resultFormat <> syncMessage

prepareWrite :: ByteString -> ByteString -> Maybe [Word32] -> Poker.Write
prepareWrite name sql parameterTypes =
  parseMessage name sql (fromMaybe [] parameterTypes) <> syncMessage

-- Sync-free variants used by the async send* functions.
-- In non-pipeline mode sendAsync appends syncMessage; in pipeline mode it does not.

asyncParamsWrite :: ByteString -> [Maybe (Word32, ByteString, Format)] -> Format -> Poker.Write
asyncParamsWrite sql params resultFormat =
  parseMessage "" sql (fmap paramOid params)
    <> bindMessage "" "" (fmap paramFormat params) (fmap paramValue params) [formatCode resultFormat]
    <> describePortalMessage ""
    <> executeMessage "" 0

asyncPreparedWrite :: ByteString -> [Maybe (ByteString, Format)] -> Format -> Poker.Write
asyncPreparedWrite name params resultFormat =
  bindMessage "" name (fmap boundFormat params) (fmap boundValue params) [formatCode resultFormat]
    <> describePortalMessage ""
    <> executeMessage "" 0

-- * Synchronous flows

-- | Simple query. Returns the last result, mirroring @PQexec@.
exec :: Connection -> ByteString -> IO (Maybe NativeResult)
exec connection sql = withReady connection do
  sendMessage connection (queryMessage sql)
  lastMaybe <$> collectSimple connection sql

-- | Parameterized query via the extended protocol.
execParams :: Connection -> ByteString -> [Maybe (Word32, ByteString, Format)] -> Format -> IO (Maybe NativeResult)
execParams connection sql params resultFormat = withReady connection do
  sendMessage connection (paramsWrite sql params resultFormat)
  Just <$> collectExtended connection sql

-- | Prepare a named statement.
prepare :: Connection -> ByteString -> ByteString -> Maybe [Word32] -> IO (Maybe NativeResult)
prepare connection name sql parameterTypes = withReady connection do
  sendMessage connection (prepareWrite name sql parameterTypes)
  Just <$> collectExtended connection sql

-- | Execute a previously prepared statement.
execPrepared :: Connection -> ByteString -> [Maybe (ByteString, Format)] -> Format -> IO (Maybe NativeResult)
execPrepared connection name params resultFormat = withReady connection do
  sendMessage connection (preparedWrite name params resultFormat)
  Just <$> collectExtended connection ""

-- * Asynchronous flows

-- | Send a write in async mode, tracking pending commands for pipeline abort.
sendAsync :: Connection -> ByteString -> Poker.Write -> IO Bool
sendAsync connection sql write = do
  status <- readIORef connection.connStatus
  case status of
    ConnectionOk -> do
      sendMessage connection write
      writeIORef connection.currentQuery sql
      writeIORef connection.asyncPending True
      pipeStatus <- readIORef connection.pipelineStatus
      when (pipeStatus /= PipelineOff) $ modifyIORef' connection.pendingCommands (+ 1)
      pure True
    _ -> pure False

-- | Whether the connection is in pipeline mode.
inPipeline :: Connection -> IO Bool
inPipeline connection = (/= PipelineOff) <$> readIORef connection.pipelineStatus

-- Simple query protocol: no Sync needed (server sends ReadyForQuery on its own).
sendQuery :: Connection -> ByteString -> IO Bool
sendQuery connection sql = sendAsync connection sql (queryMessage sql)

-- Extended query: include Sync when not in pipeline mode; omit Sync in
-- pipeline mode (the caller drives sync boundaries via 'pipelineSync').
sendQueryParams :: Connection -> ByteString -> [Maybe (Word32, ByteString, Format)] -> Format -> IO Bool
sendQueryParams connection sql params resultFormat = do
  pipeline <- inPipeline connection
  sendAsync connection sql
    $ if pipeline
      then asyncParamsWrite sql params resultFormat
      else paramsWrite sql params resultFormat

sendPrepare :: Connection -> ByteString -> ByteString -> Maybe [Word32] -> IO Bool
sendPrepare connection name sql parameterTypes = do
  pipeline <- inPipeline connection
  sendAsync connection sql
    $ if pipeline
      then parseMessage name sql (fromMaybe [] parameterTypes)
      else prepareWrite name sql parameterTypes

sendQueryPrepared :: Connection -> ByteString -> [Maybe (ByteString, Format)] -> Format -> IO Bool
sendQueryPrepared connection name params resultFormat = do
  pipeline <- inPipeline connection
  sendAsync connection ""
    $ if pipeline
      then asyncPreparedWrite name params resultFormat
      else preparedWrite name params resultFormat

sendDescribePrepared :: Connection -> ByteString -> IO Bool
sendDescribePrepared connection name = do
  pipeline <- inPipeline connection
  sendAsync connection ""
    $ if pipeline
      then describeStatementMessage name
      else describeStatementMessage name <> syncMessage

sendDescribePortal :: Connection -> ByteString -> IO Bool
sendDescribePortal connection name = do
  pipeline <- inPipeline connection
  sendAsync connection ""
    $ if pipeline
      then describePortalMessage name
      else describePortalMessage name <> syncMessage

-- | Read the next result of an in-flight asynchronous command, or 'Nothing'
-- once @ReadyForQuery@ is reached (clearing the pending flag), mirroring
-- @PQgetResult@.
--
-- In pipeline mode a separator 'Nothing' is returned between each command's
-- result set, and a 'PipelineSync' result is returned for each @Sync@
-- boundary. In single-row mode each data row is delivered as a separate
-- 'SingleTuple' result followed by a final 'TuplesOk' with no rows.
getNextResult :: Connection -> IO (Maybe NativeResult)
getNextResult connection = do
  pending <- readIORef connection.asyncPending
  if not pending
    then pure Nothing
    else do
      sepPending <- readIORef connection.pipelineSeparatorPending
      if sepPending
        then do
          writeIORef connection.pipelineSeparatorPending False
          pure Nothing
        else do
          singleRow <- readIORef connection.singleRowMode
          cachedFields <- readIORef connection.singleRowFields
          let initBuilder =
                if singleRow && not (null cachedFields)
                  then emptyBuilder {accFields = cachedFields, accSawRowDescription = True}
                  else emptyBuilder
          go singleRow initBuilder
  where
    -- Decrement the pending-command counter and set the separator flag when in
    -- pipeline mode.  Called when a "terminal" result is about to be returned.
    finishCommand pipeStatus = do
      when (pipeStatus /= PipelineOff) $ do
        modifyIORef' connection.pendingCommands (subtract 1)
        writeIORef connection.pipelineSeparatorPending True

    go singleRow builder = do
      pipeStatus <- readIORef connection.pipelineStatus
      -- In aborted pipeline mode, if the server has already sent nothing for
      -- the remaining commands (it discards them after the first error), we
      -- generate synthetic PipelineAbort results for each outstanding command
      -- rather than blocking on a wire read that will never come.
      pending <- readIORef connection.pendingCommands
      if pipeStatus == PipelineAborted && pending > 0
        then do
          modifyIORef' connection.pendingCommands (subtract 1)
          writeIORef connection.pipelineSeparatorPending True
          pure (Just (NativeResult PipelineAbort [] [] Nothing Map.empty [] ""))
        else readAndProcess singleRow builder pipeStatus

    readAndProcess singleRow builder pipeStatus = do
      message <- nextMessage connection
      case message of
        RowDescription fs ->
          go singleRow builder {accFields = fs, accSawRowDescription = True, accHadResponse = True}
        ParameterDescription oids ->
          go singleRow builder {accParamOids = oids, accHadResponse = True}
        NoData ->
          go singleRow builder {accHadResponse = True}
        DataRow values ->
          if singleRow
            then do
              writeIORef connection.singleRowFields builder.accFields
              pure (Just (NativeResult SingleTuple builder.accFields [values] Nothing Map.empty [] ""))
            else go singleRow builder {accRevRows = values : builder.accRevRows}
        ParseComplete ->
          go singleRow builder {accHadResponse = True}
        BindComplete ->
          go singleRow builder {accHadResponse = True}
        CloseComplete ->
          go singleRow builder {accHadResponse = True}
        CommandComplete tag -> do
          if singleRow
            then do
              writeIORef connection.singleRowMode False
              writeIORef connection.singleRowFields []
              writeIORef connection.lastError (Just "")
              pure (Just (NativeResult TuplesOk builder.accFields [] (Just tag) Map.empty [] ""))
            else do
              writeIORef connection.lastError (Just "")
              finishCommand pipeStatus
              pure (Just (commandResult builder (Just tag)))
        EmptyQueryResponse -> do
          writeIORef connection.lastError (Just "")
          finishCommand pipeStatus
          pure (Just (NativeResult EmptyQuery [] [] Nothing Map.empty [] ""))
        ErrorResponse fs -> do
          let errMap = Map.fromList fs
          case pipeStatus of
            PipelineAborted -> do
              -- Should not normally happen (server discards commands in abort
              -- mode) but handle defensively.
              finishCommand pipeStatus
              pure (Just (NativeResult PipelineAbort [] [] Nothing Map.empty [] ""))
            PipelineOn -> do
              writeIORef connection.pipelineStatus PipelineAborted
              finishCommand PipelineOn
              pure (Just (NativeResult FatalError [] [] Nothing errMap [] ""))
            PipelineOff -> do
              sql <- readIORef connection.currentQuery
              writeIORef connection.lastError (Just (formatResultError sql errMap))
              pure (Just (NativeResult FatalError [] [] Nothing errMap [] sql))
        PortalSuspended ->
          pure (Just (commandResult builder Nothing))
        ReadyForQuery txState -> do
          writeIORef connection.txStatus txState
          case pipeStatus of
            PipelineOff -> do
              writeIORef connection.asyncPending False
              if builder.accHadResponse
                then pure (Just (describeResult builder))
                else pure Nothing
            _ -> do
              writeIORef connection.pipelineStatus PipelineOn
              remaining <- atomicModifyIORef' connection.pendingSyncs (\n -> (n - 1, n - 1))
              if remaining == 0
                then do
                  writeIORef connection.asyncPending False
                  pure (Just (NativeResult PipelineSync [] [] Nothing Map.empty [] ""))
                else do
                  writeIORef connection.pipelineSeparatorPending True
                  pure (Just (NativeResult PipelineSync [] [] Nothing Map.empty [] ""))
        _ -> go singleRow builder

-- | Describe a prepared statement.
describePrepared :: Connection -> ByteString -> IO (Maybe NativeResult)
describePrepared connection name = withReady connection do
  sendMessage connection (describeStatementMessage name <> syncMessage)
  Just <$> collectExtended connection ""

-- | Describe a portal.
describePortal :: Connection -> ByteString -> IO (Maybe NativeResult)
describePortal connection name = withReady connection do
  sendMessage connection (describePortalMessage name <> syncMessage)
  Just <$> collectExtended connection ""

-- * Parameter projections

paramOid :: Maybe (Word32, ByteString, Format) -> Word32
paramOid = maybe 0 (\(oid, _, _) -> oid)

paramFormat :: Maybe (Word32, ByteString, Format) -> Int16
paramFormat = maybe 0 (\(_, _, format) -> formatCode format)

paramValue :: Maybe (Word32, ByteString, Format) -> Maybe ByteString
paramValue = fmap (\(_, value, _) -> value)

boundFormat :: Maybe (ByteString, Format) -> Int16
boundFormat = maybe 0 (formatCode . snd)

boundValue :: Maybe (ByteString, Format) -> Maybe ByteString
boundValue = fmap fst

formatCode :: Format -> Int16
formatCode = \case
  Text -> 0
  Binary -> 1

-- * Result collection

-- | Only run a flow on a ready connection; mirror libpq returning no result
-- when the connection is not usable.
withReady :: Connection -> IO (Maybe a) -> IO (Maybe a)
withReady connection action = do
  status <- readIORef connection.connStatus
  case status of
    ConnectionOk -> action
    _ -> pure Nothing

-- accumulator for a result under construction
data Builder = Builder
  { accFields :: [FieldDescription],
    accRevRows :: [[Maybe ByteString]],
    accParamOids :: [Word32],
    accSawRowDescription :: Bool,
    accHadResponse :: Bool
  }

emptyBuilder :: Builder
emptyBuilder = Builder [] [] [] False False

-- | Collect the (possibly several) results of a simple query, up to
-- @ReadyForQuery@. The last is what @PQexec@ returns.
-- @CopyInResponse@ and @CopyOutResponse@ terminate the loop immediately,
-- returning a synthetic result so the caller can enter the copy sub-protocol.
collectSimple :: Connection -> ByteString -> IO [NativeResult]
collectSimple connection sql = go emptyBuilder []
  where
    go builder acc = do
      message <- nextMessage connection
      case message of
        RowDescription fs -> go builder {accFields = fs, accSawRowDescription = True} acc
        DataRow values -> go builder {accRevRows = values : builder.accRevRows} acc
        CommandComplete tag -> do
          writeIORef connection.lastError (Just "")
          go emptyBuilder (commandResult builder (Just tag) : acc)
        EmptyQueryResponse -> do
          writeIORef connection.lastError (Just "")
          go emptyBuilder (NativeResult EmptyQuery [] [] Nothing Map.empty [] "" : acc)
        ErrorResponse fs -> do
          let errMap = Map.fromList fs
          writeIORef connection.lastError (Just (formatResultError sql errMap))
          go emptyBuilder (NativeResult FatalError [] [] Nothing errMap [] sql : acc)
        CopyInResponse _ formats ->
          let fields = map copyField formats
           in pure (reverse (NativeResult CopyIn fields [] Nothing Map.empty [] "" : acc))
        CopyOutResponse _ formats ->
          let fields = map copyField formats
           in pure (reverse (NativeResult CopyOut fields [] Nothing Map.empty [] "" : acc))
        ReadyForQuery txState -> do
          writeIORef connection.txStatus txState
          pure (reverse acc)
        _ -> go builder acc

-- | Collect the single result of an extended-protocol command.
collectExtended :: Connection -> ByteString -> IO NativeResult
collectExtended connection sql = go emptyBuilder Nothing
  where
    go builder finished = do
      message <- nextMessage connection
      case message of
        RowDescription fs -> go builder {accFields = fs, accSawRowDescription = True} finished
        ParameterDescription oids -> go builder {accParamOids = oids} finished
        NoData -> go builder finished
        DataRow values -> go builder {accRevRows = values : builder.accRevRows} finished
        ParseComplete -> go builder finished
        BindComplete -> go builder finished
        CloseComplete -> go builder finished
        PortalSuspended -> go emptyBuilder (finished <|> Just (commandResult builder Nothing))
        CommandComplete tag -> do
          writeIORef connection.lastError (Just "")
          go emptyBuilder (Just (commandResult builder (Just tag)))
        EmptyQueryResponse -> do
          writeIORef connection.lastError (Just "")
          go emptyBuilder (Just (NativeResult EmptyQuery [] [] Nothing Map.empty [] ""))
        ErrorResponse fs -> do
          let errMap = Map.fromList fs
          writeIORef connection.lastError (Just (formatResultError sql errMap))
          go emptyBuilder (Just (NativeResult FatalError [] [] Nothing errMap [] sql))
        ReadyForQuery txState -> do
          writeIORef connection.txStatus txState
          pure (fromMaybe (describeResult builder) finished)
        _ -> go builder finished

-- | A result terminated by @CommandComplete@\/@PortalSuspended@: 'TuplesOk' if a
-- row description was seen, else 'CommandOk'.
commandResult :: Builder -> Maybe ByteString -> NativeResult
commandResult builder tag =
  NativeResult
    (if builder.accSawRowDescription then TuplesOk else CommandOk)
    builder.accFields
    (reverse builder.accRevRows)
    tag
    Map.empty
    builder.accParamOids
    ""

-- | A result with no command completion (a @Describe@\/@Parse@-only flow):
-- 'CommandOk', carrying any column descriptions and parameter OIDs.
describeResult :: Builder -> NativeResult
describeResult builder =
  NativeResult
    CommandOk
    builder.accFields
    (reverse builder.accRevRows)
    Nothing
    Map.empty
    builder.accParamOids
    ""

lastMaybe :: [a] -> Maybe a
lastMaybe = foldl (\_ x -> Just x) Nothing

-- | Build a synthetic 'FieldDescription' from a COPY format code (0=text,
-- 1=binary).  COPY results have no column names, table OID, type OID, etc.
copyField :: Int16 -> FieldDescription
copyField fmt =
  FieldDescription
    { name = "",
      tableOid = 0,
      columnAttributeNumber = 0,
      typeOid = 0,
      typeSize = 0,
      typeModifier = 0,
      formatCode = fmt
    }
