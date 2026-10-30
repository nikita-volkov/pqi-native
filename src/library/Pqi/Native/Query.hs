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

import Control.Exception (catch, mask, mask_)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Vector.Mutable as MVector
import Pqi (ConnStatus (..), ExecStatus (..), Format (..), PipelineStatus (..))
import Pqi.Native.Connection
import Pqi.Native.Prelude
import Pqi.Native.Transport.Message
import Pqi.Native.Types (NativeResult (..), formatResultError, lostConnectionResult)
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

-- * Parameter-count limit

-- | The largest parameter count the wire protocol's 16-bit count fields can
-- carry, matching libpq's @PQ_QUERY_PARAM_MAX_LIMIT@. 'Parse' and 'Bind'
-- messages encode their parameter count as an 'Int16'; past this limit that
-- encoding wraps around instead of overflowing, so the count must be
-- rejected locally before it corrupts the message.
maxParamCount :: Int
maxParamCount = 65535

tooManyParams :: [a] -> Bool
tooManyParams = (> maxParamCount) . length

-- * Synchronous flows

-- | Simple query. Returns the last result, mirroring @PQexec@.
exec :: Connection -> ByteString -> IO (Maybe NativeResult)
exec connection sql =
  synchronously
    connection
    (queryMessage sql)
    (lastMaybe <$> collectSimple connection sql)

-- | Parameterized query via the extended protocol.
execParams :: Connection -> ByteString -> [Maybe (Word32, ByteString, Format)] -> Format -> IO (Maybe NativeResult)
execParams connection sql params resultFormat
  | tooManyParams params = pure Nothing
  | otherwise =
      synchronously
        connection
        (paramsWrite sql params resultFormat)
        (Just <$> collectExtended connection sql)

-- | Prepare a named statement.
prepare :: Connection -> ByteString -> ByteString -> Maybe [Word32] -> IO (Maybe NativeResult)
prepare connection name sql parameterTypes
  | maybe False tooManyParams parameterTypes = pure Nothing
  | otherwise =
      synchronously
        connection
        (prepareWrite name sql parameterTypes)
        (Just <$> collectExtended connection sql)

-- | Execute a previously prepared statement.
execPrepared :: Connection -> ByteString -> [Maybe (ByteString, Format)] -> Format -> IO (Maybe NativeResult)
execPrepared connection name params resultFormat
  | tooManyParams params = pure Nothing
  | otherwise =
      synchronously
        connection
        (preparedWrite name params resultFormat)
        (Just <$> collectExtended connection "")

-- * Asynchronous flows

-- | Send a write in async mode, tracking pending commands for pipeline abort.
--
-- Sending never throws. A failed write is not reported by the send but by
-- the receive that follows, as in libpq (see 'sendMessage'), so a send on a
-- usable connection returns 'True'. A send on a connection that is not
-- usable returns 'False' and records why (see 'whenUsable').
sendAsync :: Connection -> ByteString -> Poker.Write -> IO Bool
sendAsync connection sql write = whenUsable connection False do
  sendMessage connection write
  writeIORef (currentQuery connection) sql
  writeIORef (asyncPending connection) True
  pipeStatus <- readIORef (pipelineStatus connection)
  when (pipeStatus /= PipelineOff) $ modifyIORef' (pendingCommands connection) (+ 1)
  pure True

-- | Whether the connection is in pipeline mode.
inPipeline :: Connection -> IO Bool
inPipeline connection = (/= PipelineOff) <$> readIORef (pipelineStatus connection)

-- Simple query protocol: no Sync needed (server sends ReadyForQuery on its own).
sendQuery :: Connection -> ByteString -> IO Bool
sendQuery connection sql = sendAsync connection sql (queryMessage sql)

-- Extended query: include Sync when not in pipeline mode; omit Sync in
-- pipeline mode (the caller drives sync boundaries via 'pipelineSync').
sendQueryParams :: Connection -> ByteString -> [Maybe (Word32, ByteString, Format)] -> Format -> IO Bool
sendQueryParams connection sql params resultFormat
  | tooManyParams params = pure False
  | otherwise = do
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
      when (ok && pipeline) $ modifyIORef' (pendingParseOrigins connection) (Seq.|> Just False)
      pure ok

sendPrepare :: Connection -> ByteString -> ByteString -> Maybe [Word32] -> IO Bool
sendPrepare connection name sql parameterTypes
  | maybe False tooManyParams parameterTypes = pure False
  | otherwise = do
      pipeline <- inPipeline connection
      ok <-
        sendAsync connection sql
          $ if pipeline
            then parseMessage name sql (fromMaybe [] parameterTypes)
            else prepareWrite name sql parameterTypes
      when (ok && pipeline) $ modifyIORef' (pendingParseOrigins connection) (Seq.|> Just True)
      pure ok

-- | Push a @Nothing@ origin for a pipelined command that sends no @Parse@
-- (so has no @ParseComplete@ to fold or terminate on): it still occupies one
-- slot in the FIFO, popped and discarded at its own terminal message.
sendAsyncNoOrigin :: Connection -> Bool -> ByteString -> Poker.Write -> IO Bool
sendAsyncNoOrigin connection pipeline sql write = do
  ok <- sendAsync connection sql write
  when (ok && pipeline) $ modifyIORef' (pendingParseOrigins connection) (Seq.|> Nothing)
  pure ok

sendQueryPrepared :: Connection -> ByteString -> [Maybe (ByteString, Format)] -> Format -> IO Bool
sendQueryPrepared connection name params resultFormat
  | tooManyParams params = pure False
  | otherwise = do
      pipeline <- inPipeline connection
      sendAsyncNoOrigin connection pipeline ""
        $ if pipeline
          then asyncPreparedWrite name params resultFormat
          else preparedWrite name params resultFormat

sendDescribePrepared :: Connection -> ByteString -> IO Bool
sendDescribePrepared connection name = do
  pipeline <- inPipeline connection
  sendAsyncNoOrigin connection pipeline ""
    $ if pipeline
      then describeStatementMessage name
      else describeStatementMessage name <> syncMessage

sendDescribePortal :: Connection -> ByteString -> IO Bool
sendDescribePortal connection name = do
  pipeline <- inPipeline connection
  sendAsyncNoOrigin connection pipeline ""
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
--
-- Runs 'mask_'ed. The connection's result bookkeeping - the pending-command
-- counter, the separator flag, the @ParseComplete@ origin FIFO, the pipeline
-- status - lives in separate 'IORef's that a single logical transition
-- updates one after another. An async exception landing between two of those
-- updates leaves the pair inconsistent, and an inconsistent pair is not merely
-- wrong: it sends the next 'getNextResult' down the @readAndProcess@ path to
-- wait for a message the backend has already decided not to send, which is a
-- stall with no timer behind it. Masking keeps each transition atomic. The
-- blocking read inside stays interruptible (see 'Transport.receiveFrame'), so
-- this costs no abandonability.
--
-- A connection lost while waiting is reported as libpq reports it: this
-- returns the result libpq makes up for it (see 'guardLoss'), and the call
-- after that returns 'Nothing'.
getNextResult :: Connection -> IO (Maybe NativeResult)
getNextResult connection = mask_ $ guardLoss do
  pending <- readIORef (asyncPending connection)
  if not pending
    then pure Nothing
    else do
      sepPending <- readIORef (pipelineSeparatorPending connection)
      if sepPending
        then do
          writeIORef (pipelineSeparatorPending connection) False
          startNextCommand
          pure Nothing
        else do
          singleRow <- readIORef (singleRowMode connection)
          cachedFields <- readIORef (singleRowFields connection)
          builder <- newBuilder
          let initBuilder =
                if singleRow && not (null cachedFields)
                  then builder {accFields = cachedFields, accSawRowDescription = True}
                  else builder
          go singleRow initBuilder
  where
    -- Decrement the pending-command counter and set the separator flag when in
    -- pipeline mode.  Called when a "terminal" result is about to be returned.
    finishCommand pipeStatus = do
      when (pipeStatus /= PipelineOff) $ do
        modifyIORef' (pendingCommands connection) (subtract 1)
        writeIORef (pipelineSeparatorPending connection) True

    -- Where libpq moves on to the next queued command (@pqPipelineProcessQueue@)
    -- it clears the error message, which until then belongs to the command
    -- that has just been reported. Nothing is cleared when that was the last
    -- command.
    startNextCommand = do
      commands <- readIORef (pendingCommands connection)
      syncs <- readIORef (pendingSyncs connection)
      when (commands > 0 || syncs > 0) $ writeIORef (lastError connection) (Just "")

    -- Pop and discard this command's 'pendingParseOrigins' entry if it hasn't
    -- already been consumed by its own 'ParseComplete' (see the field's
    -- Haddock on 'Connection'). Called at every terminal message besides a
    -- 'ParseComplete' that itself terminates the command.
    popOriginIfPending pipeStatus builder =
      when (pipeStatus /= PipelineOff && not (accOriginPopped builder))
        $ void (popPendingParseOrigin connection)

    go singleRow builder = do
      pipeStatus <- readIORef (pipelineStatus connection)
      -- In aborted pipeline mode, if the server has already sent nothing for
      -- the remaining commands (it discards them after the first error), we
      -- generate synthetic PipelineAbort results for each outstanding command
      -- rather than blocking on a wire read that will never come.
      pending <- readIORef (pendingCommands connection)
      if pipeStatus == PipelineAborted && pending > 0
        then do
          -- This command is being synthesized without ever reading a
          -- message for it (the server sends nothing for a discarded
          -- command), so its origin - if it pushed one - is necessarily
          -- still unpopped.
          popOriginIfPending pipeStatus builder
          modifyIORef' (pendingCommands connection) (subtract 1)
          writeIORef (pipelineSeparatorPending connection) True
          pure (Just (NativeResult PipelineAbort [] [] Nothing Map.empty [] "" Nothing))
        else readAndProcess singleRow builder pipeStatus

    readAndProcess singleRow builder pipeStatus = do
      message <- receiveMessage connection
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
              pure (Just (NativeResult SingleTuple (accFields builder) [values] Nothing Map.empty [] "" Nothing))
            else do
              pushRow (accRows builder) values
              go singleRow builder
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
            Just (Just True) -> do
              finishCommand pipeStatus
              pure (Just (NativeResult CommandOk [] [] Nothing Map.empty [] "" Nothing))
            Just _ -> go singleRow builder {accHadResponse = True, accOriginPopped = True}
            Nothing -> go singleRow builder {accHadResponse = True}
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
              pure (Just (NativeResult TuplesOk (accFields builder) [] (Just tag) Map.empty [] "" Nothing))
            else do
              writeIORef (lastError connection) (Just "")
              popOriginIfPending pipeStatus builder
              finishCommand pipeStatus
              Just <$> commandResult builder (Just tag)
        EmptyQueryResponse -> do
          writeIORef (lastError connection) (Just "")
          popOriginIfPending pipeStatus builder
          finishCommand pipeStatus
          pure (Just (NativeResult EmptyQuery [] [] Nothing Map.empty [] "" Nothing))
        ErrorResponse fs -> do
          let errMap = Map.fromList fs
          case pipeStatus of
            PipelineAborted -> do
              -- Should not normally happen (server discards commands in abort
              -- mode) but handle defensively.
              popOriginIfPending pipeStatus builder
              finishCommand pipeStatus
              pure (Just (NativeResult PipelineAbort [] [] Nothing Map.empty [] "" Nothing))
            PipelineOn -> do
              writeIORef (pipelineStatus connection) PipelineAborted
              writeIORef (lastError connection) (Just (formatResultError "" errMap))
              popOriginIfPending pipeStatus builder
              finishCommand PipelineOn
              pure (Just (NativeResult FatalError [] [] Nothing errMap [] "" Nothing))
            PipelineOff -> do
              sql <- readIORef (currentQuery connection)
              writeIORef (lastError connection) (Just (formatResultError sql errMap))
              pure (Just (NativeResult FatalError [] [] Nothing errMap [] sql Nothing))
        PortalSuspended ->
          Just <$> commandResult builder Nothing
        ReadyForQuery txState -> do
          writeIORef (txStatus connection) txState
          case pipeStatus of
            PipelineOff -> do
              writeIORef (asyncPending connection) False
              if (accHadResponse builder)
                then Just <$> describeResult builder
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
              startNextCommand
              pure (Just (NativeResult PipelineSync [] [] Nothing Map.empty [] "" Nothing))
        _ -> go singleRow builder

-- | Pop the next in-flight pipelined command's origin: @Just (Just True)@ if
-- it is the terminal @ParseComplete@ of a 'sendPrepare' (to be materialized
-- as 'CommandOk'), @Just (Just False)@ if it belongs to a 'sendQueryParams'
-- and must fold into that command's accumulating result, @Just Nothing@ for
-- a command with no @Parse@ step. The outer @Nothing@ means the FIFO is
-- empty (defensive: nothing is popped and the caller folds).
popPendingParseOrigin :: Connection -> IO (Maybe (Maybe Bool))
popPendingParseOrigin connection =
  atomicModifyIORef'
    (pendingParseOrigins connection)
    ( \queue -> case Seq.viewl queue of
        origin Seq.:< rest -> (rest, Just origin)
        Seq.EmptyL -> (queue, Nothing)
    )

-- | Describe a prepared statement.
describePrepared :: Connection -> ByteString -> IO (Maybe NativeResult)
describePrepared connection name =
  synchronously
    connection
    (describeStatementMessage name <> syncMessage)
    (Just <$> collectExtended connection "")

-- | Describe a portal.
describePortal :: Connection -> ByteString -> IO (Maybe NativeResult)
describePortal connection name =
  synchronously
    connection
    (describePortalMessage name <> syncMessage)
    (Just <$> collectExtended connection "")

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

-- | Run a synchronous flow: send the write, then collect its results up to
-- @ReadyForQuery@ (or the start of a @COPY@).
--
-- First, as libpq's @PQexecStart@ does, any results of an earlier query that
-- are still unread are read and discarded, so that the flow collects its own
-- and not theirs. Those are left by a 'sendQuery' whose results were never
-- fetched, and by a synchronous flow that an asynchronous exception
-- interrupted while it waited: the connection is marked busy from the send
-- until the collection is over, so an interrupted flow leaves it busy. This
-- also lets a 'Pqi.cancel' from another thread reach the query. libpq refuses
-- a synchronous flow in pipeline mode, so there is nothing to discard there.
--
-- If the connection is lost while discarding, the flow returns no result and
-- the error message is the loss, as libpq's @PQexec@ does.
synchronously :: Connection -> Poker.Write -> IO (Maybe NativeResult) -> IO (Maybe NativeResult)
synchronously connection write collect = withReady connection do
  pipeline <- inPipeline connection
  unless pipeline discardUnread
  status <- readIORef (connStatus connection)
  if status /= ConnectionOk
    then pure Nothing
    else guardLoss $ mask \restore -> do
      sendMessage connection write
      writeIORef (asyncPending connection) True
      result <- restore collect
      writeIORef (asyncPending connection) False
      pure result
  where
    discardUnread = getNextResult connection >>= maybe (pure ()) (const discardUnread)

-- | Only run a flow on a ready connection; mirror libpq returning no result
-- when the connection is not usable, with \"no connection to the server\" as
-- the error message (see 'whenUsable').
withReady :: Connection -> IO (Maybe a) -> IO (Maybe a)
withReady connection = whenUsable connection Nothing

-- | Run a flow that reads results off the connection, and if the connection
-- is lost while it does ('receiveMessage' has recorded the loss by then),
-- return the result libpq makes up for it. This is also what @PQexec@ returns
-- when the loss comes after results that were already received, as it is the
-- last result that counts. Everything else, an asynchronous exception
-- included, propagates.
guardLoss :: IO (Maybe NativeResult) -> IO (Maybe NativeResult)
guardLoss flow =
  flow `catch` \(ConnectionLost message) -> pure (Just (lostConnectionResult message))

-- | A growable row buffer, replacing a cons-list-plus-reverse accumulation
-- with amortized-O(1) appends into a doubling mutable vector. A statement's
-- rows are pushed one 'DataRow' at a time and can number in the hundreds of
-- thousands; accumulating them as @row : rows@ and reversing at the end (the
-- prior representation) keeps two full-length cons-cell spines alive across
-- the whole statement, which dominates GC copy volume at scale (see
-- 'hasql-pqi-native-decode-gc-bound' memory). This buffer holds only the
-- array (which the RTS moves as one contiguous block, unlike a linked list)
-- until 'freezeRowBuffer' walks it once into the final list.
data RowBuffer
  = RowBuffer
      -- | The mutable vector of rows.
      !(IORef (MVector.IOVector [Maybe ByteString]))
      -- | Length.
      !(IORef Int)

newRowBuffer :: IO RowBuffer
newRowBuffer = do
  v <- MVector.new 64
  RowBuffer <$> newIORef v <*> newIORef 0

pushRow :: RowBuffer -> [Maybe ByteString] -> IO ()
pushRow (RowBuffer vRef lenRef) row = do
  v <- readIORef vRef
  len <- readIORef lenRef
  v' <-
    if len >= MVector.length v
      then do
        grown <- MVector.grow v (MVector.length v)
        writeIORef vRef grown
        pure grown
      else pure v
  MVector.write v' len row
  writeIORef lenRef (len + 1)

-- | Walk the buffer back-to-front once, consing onto an accumulator, which
-- yields the rows in their original order without a separate reverse pass.
freezeRowBuffer :: RowBuffer -> IO [[Maybe ByteString]]
freezeRowBuffer (RowBuffer vRef lenRef) = do
  v <- readIORef vRef
  len <- readIORef lenRef
  let go !i acc
        | i < 0 = pure acc
        | otherwise = do
            x <- MVector.read v i
            go (i - 1) (x : acc)
  go (len - 1) []

-- accumulator for a result under construction
data Builder = Builder
  { accFields :: [FieldDescription],
    accRows :: RowBuffer,
    accParamOids :: [Word32],
    accSawRowDescription :: Bool,
    accHadResponse :: Bool,
    -- | Whether this command's 'pendingParseOrigins' entry has already been
    -- popped (via its own 'ParseComplete'). See 'popOriginIfPending'.
    accOriginPopped :: Bool
  }

newBuilder :: IO Builder
newBuilder = do
  rows <- newRowBuffer
  pure (Builder [] rows [] False False False)

-- | Collect the (possibly several) results of a simple query, up to
-- @ReadyForQuery@. The last is what @PQexec@ returns.
-- @CopyInResponse@ and @CopyOutResponse@ terminate the loop immediately,
-- returning a synthetic result so the caller can enter the copy sub-protocol.
collectSimple :: Connection -> ByteString -> IO [NativeResult]
collectSimple connection sql = do
  builder0 <- newBuilder
  go builder0 []
  where
    go builder acc = do
      message <- receiveMessage connection
      case message of
        RowDescription fs -> go builder {accFields = fs, accSawRowDescription = True} acc
        DataRow values -> do
          pushRow (accRows builder) values
          go builder acc
        CommandComplete tag -> do
          writeIORef (lastError connection) (Just "")
          result <- commandResult builder (Just tag)
          builder' <- newBuilder
          go builder' (result : acc)
        EmptyQueryResponse -> do
          writeIORef (lastError connection) (Just "")
          builder' <- newBuilder
          go builder' (NativeResult EmptyQuery [] [] Nothing Map.empty [] "" Nothing : acc)
        ErrorResponse fs -> do
          let errMap = Map.fromList fs
          writeIORef (lastError connection) (Just (formatResultError sql errMap))
          builder' <- newBuilder
          go builder' (NativeResult FatalError [] [] Nothing errMap [] sql Nothing : acc)
        CopyInResponse _ formats ->
          let fields = map copyField formats
           in pure (reverse (NativeResult CopyIn fields [] Nothing Map.empty [] "" Nothing : acc))
        CopyOutResponse _ formats ->
          let fields = map copyField formats
           in pure (reverse (NativeResult CopyOut fields [] Nothing Map.empty [] "" Nothing : acc))
        ReadyForQuery txState -> do
          writeIORef (txStatus connection) txState
          pure (reverse acc)
        _ -> go builder acc

-- | Collect the single result of an extended-protocol command.
collectExtended :: Connection -> ByteString -> IO NativeResult
collectExtended connection sql = do
  builder0 <- newBuilder
  go builder0 Nothing
  where
    go builder finished = do
      message <- receiveMessage connection
      case message of
        RowDescription fs -> go builder {accFields = fs, accSawRowDescription = True} finished
        ParameterDescription oids -> go builder {accParamOids = oids} finished
        NoData -> go builder finished
        DataRow values -> do
          pushRow (accRows builder) values
          go builder finished
        ParseComplete -> go builder finished
        BindComplete -> go builder finished
        CloseComplete -> go builder finished
        PortalSuspended -> do
          finished' <- case finished of
            Just _ -> pure finished
            Nothing -> Just <$> commandResult builder Nothing
          builder' <- newBuilder
          go builder' finished'
        CommandComplete tag -> do
          writeIORef (lastError connection) (Just "")
          result <- commandResult builder (Just tag)
          builder' <- newBuilder
          go builder' (Just result)
        EmptyQueryResponse -> do
          writeIORef (lastError connection) (Just "")
          builder' <- newBuilder
          go builder' (Just (NativeResult EmptyQuery [] [] Nothing Map.empty [] "" Nothing))
        ErrorResponse fs -> do
          let errMap = Map.fromList fs
          writeIORef (lastError connection) (Just (formatResultError sql errMap))
          builder' <- newBuilder
          go builder' (Just (NativeResult FatalError [] [] Nothing errMap [] sql Nothing))
        ReadyForQuery txState -> do
          writeIORef (txStatus connection) txState
          case finished of
            Just result -> pure result
            Nothing -> describeResult builder
        _ -> go builder finished

-- | A result terminated by @CommandComplete@\/@PortalSuspended@: 'TuplesOk' if a
-- row description was seen, else 'CommandOk'.
commandResult :: Builder -> Maybe ByteString -> IO NativeResult
commandResult builder tag = do
  rows <- freezeRowBuffer (accRows builder)
  pure
    $ NativeResult
      (if (accSawRowDescription builder) then TuplesOk else CommandOk)
      (accFields builder)
      rows
      tag
      Map.empty
      (accParamOids builder)
      ""
      Nothing

-- | A result with no command completion (a @Describe@\/@Parse@-only flow):
-- 'CommandOk', carrying any column descriptions and parameter OIDs.
describeResult :: Builder -> IO NativeResult
describeResult builder = do
  rows <- freezeRowBuffer (accRows builder)
  pure
    $ NativeResult
      CommandOk
      (accFields builder)
      rows
      Nothing
      Map.empty
      (accParamOids builder)
      ""
      Nothing

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
