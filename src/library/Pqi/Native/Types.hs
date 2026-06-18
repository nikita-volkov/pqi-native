-- | Internal types for the native adapter, separated from 'Connection.hs' to
-- avoid orphan-instance warnings for 'IsResult' and 'IsCancel'.
module Pqi.Native.Types
  ( NativeResult (..),
    NativeCancel (..),
    formatErrorFields,
    formatResultError,
  )
where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.Char (isDigit)
import Data.List (findIndex)
import qualified Data.Map.Strict as Map
import Pqi
  ( ExecStatus (..),
    FieldCode (..),
    Format (..),
    IsCancel (..),
    IsResult (..),
    PipelineStatus (..),
  )
import Pqi.Native.Prelude
import qualified Pqi.Native.Transport as Transport
import Pqi.Native.Transport.Message (FieldDescription (..), cancelRequest)
import Control.Exception (IOException, try)

-- | A fully materialized result. The native adapter buffers the entire result
-- in memory, so the accessors are pure lookups and 'unsafeFreeResult' is a
-- no-op.
data NativeResult = NativeResult
  { status :: ExecStatus,
    fields :: [FieldDescription],
    rows :: [[Maybe ByteString]],
    commandTag :: Maybe ByteString,
    errorFields :: Map.Map Word8 ByteString,
    paramOids :: [Word32],
    -- | The query text that produced this result (used to format @LINE N:@
    -- position context in 'resultErrorMessage', matching libpq's behaviour).
    -- Empty for non-error results and for async results where the query is
    -- unavailable.
    queryText :: ByteString
  }
  deriving stock (Eq, Show)

-- | A standalone cancellation handle for the native adapter.
--
-- Carries references to connection state so the cancel implementation can
-- decide whether a network round-trip is necessary:
--
-- * @asyncPendingRef@: False when nothing is in flight at all — skip the
--   round-trip entirely.
-- * @pipelineStatusRef@ + @pendingCommandsRef@: in pipeline mode,
--   @asyncPending@ stays True even after all @CommandComplete@ messages
--   have arrived (while @ReadyForQuery@ is still unread).  Sending a cancel
--   in that window produces a stale signal that can interrupt the very next
--   command (e.g. the @ABORT@ issued during clean-up).  Checking
--   @pendingCommands > 0@ instead avoids the stale cancel: once all command
--   completions are in, @pendingCommands@ is 0 and the server is idle.
data NativeCancel = NativeCancel
  { host :: ByteString,
    port :: Int,
    pid :: Int32,
    secret :: Int32,
    asyncPendingRef :: IORef Bool,
    pipelineStatusRef :: IORef PipelineStatus,
    pendingCommandsRef :: IORef Int
  }

-- | Like 'formatResultError' but without a client query text (for
-- connection-level errors, which never carry a statement-position field).
formatErrorFields :: Map.Map Word8 ByteString -> ByteString
formatErrorFields = formatResultError ""

-- | Format error\/notice fields into a message string matching libpq's
-- @PQresultErrorMessage@ at DEFAULT verbosity with @SHOW_CONTEXT_ERRORS@
-- visibility.
--
-- Field order (matching libpq):
-- 1. @\<Severity\>:  \<Message\>\\n@
-- 2. @LINE N: \<query line\>\\n        \^\\n@ (statement position via @queryText@ + @\'P\'@ field)
-- 3. @LINE N: \<internal query\>\\n        \^\\nQUERY: \<internal query\>\\n@ (@\'q\'@+@\'p\'@ fields)
-- 4. @DETAIL:  \<D\>\\n@
-- 5. @HINT:  \<H\>\\n@
-- 6. @CONTEXT:  \<W\>\\n@ (ERROR\/FATAL\/PANIC only)
formatResultError :: ByteString -> Map.Map Word8 ByteString -> ByteString
formatResultError queryText fields =
  case Map.lookup 0x4d fields of
    Nothing -> ""
    Just msg ->
      let sev = Map.findWithDefault "" 0x53 fields
          isError = sev `elem` ["ERROR", "FATAL", "PANIC"]
          line1 = (if ByteString.null sev then msg else sev <> ":  " <> msg) <> "\n"
          -- Statement position ('P', 0x50): needs client query text
          stmtCtx = case Map.lookup 0x50 fields of
            Just posStr
              | not (ByteString.null queryText) ->
                  maybe "" (positionContext queryText) (readPositiveInt posStr)
            _ -> ""
          -- Internal query+position ('q'=0x71, 'p'=0x70): both in wire fields
          intCtx = case (Map.lookup 0x71 fields, Map.lookup 0x70 fields) of
            (Just intQuery, Just posStr) ->
              maybe
                ""
                (\p -> positionContext intQuery p <> "QUERY:  " <> intQuery <> "\n")
                (readPositiveInt posStr)
            _ -> ""
          detLine = case Map.lookup 0x44 fields of
            Just det -> "DETAIL:  " <> det <> "\n"
            _ -> ""
          hntLine = case Map.lookup 0x48 fields of
            Just hnt -> "HINT:  " <> hnt <> "\n"
            _ -> ""
          ctxLine = case Map.lookup 0x57 fields of
            Just ctx | isError -> "CONTEXT:  " <> ctx <> "\n"
            _ -> ""
       in line1 <> stmtCtx <> intCtx <> detLine <> hntLine <> ctxLine

-- | Build the @LINE N: \<text\>\\n        \^\\n@ block for a 1-indexed position
-- within a query string, matching libpq's formatting exactly.
positionContext :: ByteString -> Int -> ByteString
positionContext query pos =
  let pos0 = max 0 (pos - 1)
      before = ByteString.take pos0 query
      lineNum = 1 + ByteString.length (ByteString.filter (== 0x0a) before)
      lineStart = maybe 0 (+ 1) (ByteString.elemIndexEnd 0x0a before)
      col = pos0 - lineStart
      rest = ByteString.drop lineStart query
      lineText = ByteString.takeWhile (/= 0x0a) rest
      prefix = ByteString.Char8.pack ("LINE " <> show lineNum <> ": ")
      caret = ByteString.replicate (ByteString.length prefix + col) 0x20 <> "^\n"
   in prefix <> lineText <> "\n" <> caret

readPositiveInt :: ByteString -> Maybe Int
readPositiveInt bs = case ByteString.Char8.readInt bs of
  Just (n, _) | n > 0 -> Just n
  _ -> Nothing

instance IsResult NativeResult where
  resultStatus result = pure result.status
  resultErrorMessage result = pure (Just (formatResultError result.queryText result.errorFields))
  resultErrorField result field = pure (Map.lookup (fieldCodeByte field) result.errorFields)
  unsafeFreeResult _ = pure ()
  ntuples result = pure (fromIntegral (length result.rows))
  nfields result = pure (fromIntegral (length result.fields))
  fname result column = pure $ do
    fd <- atMay result.fields column
    if ByteString.null fd.name then Nothing else Just fd.name
  fnumber result name =
    pure (fromIntegral <$> findIndex (\field -> field.name == folded) result.fields)
    where
      folded = foldIdentifier name
  ftable result column = pure (maybe 0 (.tableOid) (atMay result.fields column))
  ftablecol result column =
    pure (maybe 0 (\field -> fromIntegral (field.columnAttributeNumber :: Int16)) (atMay result.fields column))
  fformat result column =
    pure (maybe Text (\field -> formatOf field.formatCode) (atMay result.fields column))
  ftype result column = pure (maybe 0 (.typeOid) (atMay result.fields column))
  fmod result column = pure (maybe 0 (\field -> fromIntegral (field.typeModifier :: Int32)) (atMay result.fields column))
  fsize result column = pure (maybe 0 (\field -> fromIntegral (field.typeSize :: Int16)) (atMay result.fields column))
  getvalue result row column = pure (join (cellAt result row column))
  getvalue' result row column = pure (join (cellAt result row column))
  getisnull result row column = pure (maybe True isNothing (cellAt result row column))
  getlength result row column =
    pure (maybe 0 (maybe 0 ByteString.length) (cellAt result row column))
  nparams result = pure (fromIntegral (length result.paramOids))
  paramtype result index = pure (fromMaybe 0 (atMay result.paramOids index))
  cmdStatus result = pure (Just (fromMaybe "" result.commandTag))
  cmdTuples result = pure (Just (maybe "" affectedRows result.commandTag))

instance IsCancel NativeCancel where
  cancel nc = do
    pending <- readIORef nc.asyncPendingRef
    if not pending
      then pure (Right ())
      else do
        transport <- Transport.connect nc.host nc.port
        Transport.send transport (cancelRequest nc.pid nc.secret)
        -- Read until EOF to ensure the server has processed the cancel request
        -- before we close the connection. This matches libpq's PQcancel behavior
        -- and prevents the cancel signal from racing with the next query.
        _ <- try @IOException (Transport.readUntilClosed transport)
        Transport.close transport
        pure (Right ())

-- * Helpers for the 'IsResult' instance

atMay :: [a] -> Int32 -> Maybe a
atMay xs i
  | i < 0 = Nothing
  | otherwise = case drop (fromIntegral i) xs of
      x : _ -> Just x
      [] -> Nothing

cellAt :: NativeResult -> Int32 -> Int32 -> Maybe (Maybe ByteString)
cellAt result row column = do
  rowValues <- atMay result.rows row
  atMay rowValues column

formatOf :: Int16 -> Format
formatOf = \case
  1 -> Binary
  _ -> Text

foldIdentifier :: ByteString -> ByteString
foldIdentifier = ByteString.pack . outside . ByteString.unpack
  where
    quote = 0x22
    outside = \case
      [] -> []
      c : rest
        | c == quote -> inside rest
        | otherwise -> asciiToLower c : outside rest
    inside = \case
      [] -> []
      c : rest
        | c == quote -> case rest of
            c' : rest' | c' == quote -> quote : inside rest'
            _ -> outside rest
        | otherwise -> c : inside rest
    asciiToLower c
      | c >= 0x41 && c <= 0x5a = c + 0x20
      | otherwise = c

fieldCodeByte :: FieldCode -> Word8
fieldCodeByte = \case
  DiagSeverity -> 0x53 -- 'S'
  DiagSqlstate -> 0x43 -- 'C'
  DiagMessagePrimary -> 0x4d -- 'M'
  DiagMessageDetail -> 0x44 -- 'D'
  DiagMessageHint -> 0x48 -- 'H'
  DiagStatementPosition -> 0x50 -- 'P'
  DiagInternalPosition -> 0x70 -- 'p'
  DiagInternalQuery -> 0x71 -- 'q'
  DiagContext -> 0x57 -- 'W'
  DiagSourceFile -> 0x46 -- 'F'
  DiagSourceLine -> 0x4c -- 'L'
  DiagSourceFunction -> 0x52 -- 'R'

-- | The affected-row count from a @CommandComplete@ tag, matching
-- @PQcmdTuples@: the last whitespace-delimited token if it is all digits (which
-- covers @INSERT oid rows@, @UPDATE n@, @SELECT n@, …), else empty.
affectedRows :: ByteString -> ByteString
affectedRows tag =
  case ByteString.Char8.words tag of
    [] -> ""
    tokens ->
      let final = last tokens
       in if not (ByteString.null final) && ByteString.Char8.all isDigit final
            then final
            else ""
