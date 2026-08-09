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
import qualified Data.Sequence as Seq
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
    <> bindMessage "" "" (fmap paramFormat params) (fmap paramValue params) [formatCodeOf resultFormat]
    <> describePortalMessage ""
    <> executeMessage "" 0

asyncPreparedWrite :: ByteString -> [Maybe (ByteString, Format)] -> Format -> Poker.Write
asyncPreparedWrite name params resultFormat =
  bindMessage "" name (fmap boundFormat params) (fmap boundValue params) [formatCodeOf resultFormat]
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
  status <- readIORef (connStatus connection)
  case status of
    ConnectionOk -> do
      sendMessage connection write
      writeIORef (currentQuery connection) sql
      writeIORef (asyncPending connection) True
      pipeStatus <- readIORef (pipelineStatus connection)
      when (pipeStatus /= PipelineOff) $ modifyIORef' (pendingCommands connection) (+ 1)
      pure True
    _ -> pure False

-- | Whether the connection is in pipeline mode.
inPipeline :: Connection -> IO Bool
inPipeline connection = (/= PipelineOff) <$> readIORef (pipelineStatus connection)

-- Simple query protocol: no Sync needed (server sends ReadyForQuery on its own).
sendQuery :: Connection -> ByteString -> IO Bool
sendQuery connection sql = sendAsync connection sql (queryMessage sql)

-- Extended query: include Sync when not in pipeline mode; omit Sync in
-- pipeline mode (the caller drives sync boundaries via 'pipelineSync').
sendQueryParams :: Connection -> ByteString -> [Maybe (Word32, ByteString, Format)] -> Format -> IO Bool
sendQueryParams connection sql params resultFormat = do
  pipeline <- inPipeline connection
  ok <-
    sendAsync connection sql
      $ if pipeline
        then asyncParamsWrite sql params resultFormat
        else paramsWrite sql params resultFormat
  -- 'sendQueryParams' drives the extended protocol and so always sends an
  -- unnamed @Parse@, whose @ParseComplete@ must fold into this command's own
  -- result (like 'sendQueryPrepared'). Record its origin so it is not mistaken
  -- for the terminal @ParseComplete@ of a pipelined 'sendPrepare'.
  when (ok && pipeline) $ modifyIORef' (pendingParseOrigins connection) (Seq.|> False)
  pure ok

sendPrepare :: Connection -> ByteString -> ByteString -> Maybe [Word32] -> IO Bool
sendPrepare connection name sql parameterTypes = do
  pipeline <- inPipeline connection
  ok <-
    sendAsync connection sql
      $ if pipeline
        then parseMessage name sql (fromMaybe [] parameterTypes)
        else prepareWrite name sql parameterTypes
  when (ok && pipeline) $ modifyIORef' (pendingParseOrigins connection) (Seq.|> True)
  pure ok

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
  pending <- readIORef (asyncPending connection)
  if not pending
    then pure Nothing
    else do
      sepPending <- readIORef (pipelineSeparatorPending connection)
      if sepPending
        then do
          writeIORef (pipelineSeparatorPending connection) False
          pure Nothing
        else do
          singleRow <- readIORef (singleRowMode connection)
          cachedFields <- readIORef (singleRowFields connection)
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
        modifyIORef' (pendingCommands connection) (subtract 1)
        writeIORef (pipelineSeparatorPending connection) True

    go singleRow builder = do
      pipeStatus <- readIORef (pipelineStatus connection)
      -- In aborted pipeline mode, if the server has already sent nothing for
      -- the remaining commands (it discards them after the first error), we
      -- generate synthetic PipelineAbort results for each outstanding command
      -- rather than blocking on a wire read that will never come.
      pending <- readIORef (pendingCommands connection)
      if pipeStatus == PipelineAborted && pending > 0
        then do
          modifyIORef' (pendingCommands connection) (subtract 1)
          writeIORef (pipelineSeparatorPending connection) True
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
              writeIORef (singleRowFields connection) (accFields builder)
              pure (Just (NativeResult SingleTuple (accFields builder) [values] Nothing Map.empty [] ""))
            else go singleRow builder {accRevRows = values : (accRevRows builder)}
        ParseComplete -> do
          -- Charge the @ParseComplete@ to the command that produced it by
          -- popping the oldest recorded origin. Only a 'sendPrepare' origin
          -- (@True@) terminates the command as 'CommandOk'; a 'sendQueryParams'
          -- origin (@False@) folds into the accumulating result, and so does a
          -- @ParseComplete@ seen outside pipeline mode (e.g. a non-pipelined
          -- async 'sendQueryParams', whose result is collected by 'collectExtended').
          origin <-
            if pipeStatus /= PipelineOff
              then popPendingParseOrigin connection
              else pure Nothing
          case origin of
            Just True -> do
              finishCommand pipeStatus
              pure (Just (NativeResult CommandOk [] [] Nothing Map.empty [] ""))
            _ -> go singleRow builder {accHadResponse = True}
        BindComplete ->
          go singleRow builder {accHadResponse = True}
        CloseComplete ->
          go singleRow builder {accHadResponse = True}
        CommandComplete tag -> do
          if singleRow
            then do
              writeIORef (singleRowMode connection) False
              writeIORef (singleRowFields connection) []
              writeIORef (lastError connection) (Just "")
              pure (Just (NativeResult TuplesOk (accFields builder) [] (Just tag) Map.empty [] ""))
            else do
              writeIORef (lastError connection) (Just "")
              finishCommand pipeStatus
              pure (Just (commandResult builder (Just tag)))
        EmptyQueryResponse -> do
          writeIORef (lastError connection) (Just "")
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
              writeIORef (pipelineStatus connection) PipelineAborted
              finishCommand PipelineOn
              pure (Just (NativeResult FatalError [] [] Nothing errMap [] ""))
            PipelineOff -> do
              sql <- readIORef (currentQuery connection)
              writeIORef (lastError connection) (Just (formatResultError sql errMap))
              pure (Just (NativeResult FatalError [] [] Nothing errMap [] sql))
        PortalSuspended ->
          pure (Just (commandResult builder Nothing))
        ReadyForQuery txState -> do
          writeIORef (txStatus connection) txState
          case pipeStatus of
            PipelineOff -> do
              writeIORef (asyncPending connection) False
              if (accHadResponse builder)
                then pure (Just (describeResult builder))
                else pure Nothing
            _ -> do
              writeIORef (pipelineStatus connection) PipelineOn
              -- A PipelineSync result is its own command boundary: unlike a
              -- normal command result, libpq does not emit a separating NULL
              -- after it, so consecutive syncs are reported back-to-back. We
              -- therefore never set 'pipelineSeparatorPending' here. Only the
              -- final sync clears 'asyncPending'; an earlier one leaves it set
              -- so the next 'getNextResult' reads straight on to the next sync.
              remaining <- atomicModifyIORef' (pendingSyncs connection) (\n -> (n - 1, n - 1))
              when (remaining == 0) $ writeIORef (asyncPending connection) False
              pure (Just (NativeResult PipelineSync [] [] Nothing Map.empty [] ""))
        _ -> go singleRow builder

-- | Pop the origin of the next in-flight @ParseComplete@ in pipeline mode:
-- @True@ if it is the terminal @ParseComplete@ of a 'sendPrepare' (to be
-- materialized as 'CommandOk'), @False@ if it belongs to a 'sendQueryParams'
-- and must fold into that command's accumulating result. @Nothing@ when no
-- @Parse@ is pending (defensive: the @ParseComplete@ is then folded).
popPendingParseOrigin :: Connection -> IO (Maybe Bool)
popPendingParseOrigin connection =
  atomicModifyIORef'
    (pendingParseOrigins connection)
    ( \queue -> case Seq.viewl queue of
        origin Seq.:< rest -> (rest, Just origin)
        Seq.EmptyL -> (queue, Nothing)
    )

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
paramFormat = maybe 0 (\(_, _, format) -> formatCodeOf format)

paramValue :: Maybe (Word32, ByteString, Format) -> Maybe ByteString
paramValue = fmap (\(_, value, _) -> value)

boundFormat :: Maybe (ByteString, Format) -> Int16
boundFormat = maybe 0 (formatCodeOf . snd)

boundValue :: Maybe (ByteString, Format) -> Maybe ByteString
boundValue = fmap fst

-- | Renamed from @formatCode@ to avoid clashing with 'FieldDescription's
-- @formatCode@ field now that 'DuplicateRecordFields' is no longer enabled.
formatCodeOf :: Format -> Int16
formatCodeOf = \case
  Text -> 0
  Binary -> 1

-- * Result collection

-- | Only run a flow on a ready connection; mirror libpq returning no result
-- when the connection is not usable.
withReady :: Connection -> IO (Maybe a) -> IO (Maybe a)
withReady connection action = do
  status <- readIORef (connStatus connection)
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
        DataRow values -> go builder {accRevRows = values : (accRevRows builder)} acc
        CommandComplete tag -> do
          writeIORef (lastError connection) (Just "")
          go emptyBuilder (commandResult builder (Just tag) : acc)
        EmptyQueryResponse -> do
          writeIORef (lastError connection) (Just "")
          go emptyBuilder (NativeResult EmptyQuery [] [] Nothing Map.empty [] "" : acc)
        ErrorResponse fs -> do
          let errMap = Map.fromList fs
          writeIORef (lastError connection) (Just (formatResultError sql errMap))
          go emptyBuilder (NativeResult FatalError [] [] Nothing errMap [] sql : acc)
        CopyInResponse _ formats ->
          let fields = map copyField formats
           in pure (reverse (NativeResult CopyIn fields [] Nothing Map.empty [] "" : acc))
        CopyOutResponse _ formats ->
          let fields = map copyField formats
           in pure (reverse (NativeResult CopyOut fields [] Nothing Map.empty [] "" : acc))
        ReadyForQuery txState -> do
          writeIORef (txStatus connection) txState
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
        DataRow values -> go builder {accRevRows = values : (accRevRows builder)} finished
        ParseComplete -> go builder finished
        BindComplete -> go builder finished
        CloseComplete -> go builder finished
        PortalSuspended -> go emptyBuilder (finished <|> Just (commandResult builder Nothing))
        CommandComplete tag -> do
          writeIORef (lastError connection) (Just "")
          go emptyBuilder (Just (commandResult builder (Just tag)))
        EmptyQueryResponse -> do
          writeIORef (lastError connection) (Just "")
          go emptyBuilder (Just (NativeResult EmptyQuery [] [] Nothing Map.empty [] ""))
        ErrorResponse fs -> do
          let errMap = Map.fromList fs
          writeIORef (lastError connection) (Just (formatResultError sql errMap))
          go emptyBuilder (Just (NativeResult FatalError [] [] Nothing errMap [] sql))
        ReadyForQuery txState -> do
          writeIORef (txStatus connection) txState
          pure (fromMaybe (describeResult builder) finished)
        _ -> go builder finished

-- | A result terminated by @CommandComplete@\/@PortalSuspended@: 'TuplesOk' if a
-- row description was seen, else 'CommandOk'.
commandResult :: Builder -> Maybe ByteString -> NativeResult
commandResult builder tag =
  NativeResult
    (if (accSawRowDescription builder) then TuplesOk else CommandOk)
    (accFields builder)
    (reverse (accRevRows builder))
    tag
    Map.empty
    (accParamOids builder)
    ""

-- | A result with no command completion (a @Describe@\/@Parse@-only flow):
-- 'CommandOk', carrying any column descriptions and parameter OIDs.
describeResult :: Builder -> NativeResult
describeResult builder =
  NativeResult
    CommandOk
    (accFields builder)
    (reverse (accRevRows builder))
    Nothing
    Map.empty
    (accParamOids builder)
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
