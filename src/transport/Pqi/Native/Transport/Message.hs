-- | The PostgreSQL frontend\/backend wire messages: builders ('Poker.Write') for
-- the frontend messages we send, and a decoder for the backend messages we
-- receive. Message framing (@[type byte][Int32 length][body]@, with the
-- startup message lacking a type byte) lives here too.
module Pqi.Native.Transport.Message
  ( -- * Frontend messages
    startupMessage,
    sslRequest,
    cancelRequest,
    passwordMessage,
    saslInitialResponse,
    saslResponse,
    queryMessage,
    parseMessage,
    bindMessage,
    describeStatementMessage,
    describePortalMessage,
    executeMessage,
    closeStatementMessage,
    closePortalMessage,
    syncMessage,
    flushMessage,
    terminateMessage,
    copyDataMessage,
    copyDoneMessage,
    copyFailMessage,

    -- * Backend messages
    BackendMessage (..),
    FieldDescription (..),
    decodeBackendMessage,
  )
where

import qualified Data.ByteString as ByteString
import Pqi.Native.Comms
import Pqi.Native.Transport.Prelude
import qualified PtrPoker.Write as Poker

-- * Framing helpers

-- | Frame a typed frontend message: type byte, then a length covering the
-- length field and the body, then the body.
framed :: Word8 -> Poker.Write -> Poker.Write
framed typeByte body =
  Poker.word8 typeByte
    <> Poker.bInt32 (fromIntegral (4 + Poker.writeSize body))
    <> body

-- | A null-terminated string.
cstr :: ByteString -> Poker.Write
cstr value = Poker.byteString value <> Poker.word8 0

formatCodes :: [Int16] -> Poker.Write
formatCodes codes =
  Poker.bInt16 (fromIntegral (length codes)) <> foldMap Poker.bInt16 codes

-- * Frontend messages

-- | The startup message (no type byte), requesting protocol 3.0 with the given
-- parameters (e.g. @user@, @database@).
startupMessage :: [(ByteString, ByteString)] -> Poker.Write
startupMessage parameters =
  let body =
        Poker.bInt32 196608 -- protocol version 3.0 == (3 << 16)
          <> foldMap (\(k, v) -> cstr k <> cstr v) parameters
          <> Poker.word8 0
   in Poker.bInt32 (fromIntegral (4 + Poker.writeSize body)) <> body

-- | The SSL negotiation request (no type byte).
sslRequest :: Poker.Write
sslRequest = Poker.bInt32 8 <> Poker.bInt32 80877103

-- | A cancel request (no type byte), sent on a fresh connection: the cancel
-- request code followed by the target backend's process ID and secret key.
cancelRequest :: Int32 -> Int32 -> Poker.Write
cancelRequest processId secretKey =
  Poker.bInt32 16 <> Poker.bInt32 80877102 <> Poker.bInt32 processId <> Poker.bInt32 secretKey

-- | A cleartext or MD5 password response (message type @p@).
passwordMessage :: ByteString -> Poker.Write
passwordMessage password = framed 0x70 (cstr password)

-- | A SASL initial response (message type @p@): the chosen mechanism and the
-- client-first message.
saslInitialResponse :: ByteString -> ByteString -> Poker.Write
saslInitialResponse mechanism initialData =
  framed 0x70
    $ cstr mechanism
    <> Poker.bInt32 (fromIntegral (ByteString.length initialData))
    <> Poker.byteString initialData

-- | A subsequent SASL response (message type @p@).
saslResponse :: ByteString -> Poker.Write
saslResponse responseData = framed 0x70 (Poker.byteString responseData)

-- | A simple @Query@ message.
queryMessage :: ByteString -> Poker.Write
queryMessage sql = framed 0x51 (cstr sql)

-- | A @Parse@ message: statement name, SQL, and parameter type OIDs.
parseMessage :: ByteString -> ByteString -> [Word32] -> Poker.Write
parseMessage name sql oids =
  framed 0x50
    $ cstr name
    <> cstr sql
    <> Poker.bInt16 (fromIntegral (length oids))
    <> foldMap Poker.bWord32 oids

-- | A @Bind@ message: portal, statement, per-parameter format codes, parameter
-- values ('Nothing' for SQL @NULL@), and result format codes.
bindMessage :: ByteString -> ByteString -> [Int16] -> [Maybe ByteString] -> [Int16] -> Poker.Write
bindMessage portal statement parameterFormats values resultFormats =
  framed 0x42
    $ cstr portal
    <> cstr statement
    <> formatCodes parameterFormats
    <> Poker.bInt16 (fromIntegral (length values))
    <> foldMap bindValue values
    <> formatCodes resultFormats
  where
    bindValue = \case
      Nothing -> Poker.bInt32 (-1)
      Just value -> Poker.bInt32 (fromIntegral (ByteString.length value)) <> Poker.byteString value

-- | A @Describe@ message targeting a prepared statement.
describeStatementMessage :: ByteString -> Poker.Write
describeStatementMessage name = framed 0x44 (Poker.word8 0x53 <> cstr name)

-- | A @Describe@ message targeting a portal.
describePortalMessage :: ByteString -> Poker.Write
describePortalMessage name = framed 0x44 (Poker.word8 0x50 <> cstr name)

-- | An @Execute@ message: portal name and a maximum row count (0 = unlimited).
executeMessage :: ByteString -> Int32 -> Poker.Write
executeMessage portal maxRows = framed 0x45 (cstr portal <> Poker.bInt32 maxRows)

-- | A @Close@ message targeting a prepared statement.
closeStatementMessage :: ByteString -> Poker.Write
closeStatementMessage name = framed 0x43 (Poker.word8 0x53 <> cstr name)

-- | A @Close@ message targeting a portal.
closePortalMessage :: ByteString -> Poker.Write
closePortalMessage name = framed 0x43 (Poker.word8 0x50 <> cstr name)

-- | A @Sync@ message.
syncMessage :: Poker.Write
syncMessage = framed 0x53 mempty

-- | A @Flush@ message.
flushMessage :: Poker.Write
flushMessage = framed 0x48 mempty

-- | A @Terminate@ message.
terminateMessage :: Poker.Write
terminateMessage = framed 0x58 mempty

-- | A @CopyData@ message.
copyDataMessage :: ByteString -> Poker.Write
copyDataMessage payload = framed 0x64 (Poker.byteString payload)

-- | A @CopyDone@ message.
copyDoneMessage :: Poker.Write
copyDoneMessage = framed 0x63 mempty

-- | A @CopyFail@ message with an error string.
copyFailMessage :: ByteString -> Poker.Write
copyFailMessage reason = framed 0x66 (cstr reason)

-- * Backend messages

-- | The description of one result column, from a @RowDescription@ message.
data FieldDescription = FieldDescription
  { name :: ByteString,
    tableOid :: Word32,
    columnAttributeNumber :: Int16,
    typeOid :: Word32,
    typeSize :: Int16,
    typeModifier :: Int32,
    formatCode :: Int16
  }
  deriving stock (Eq, Show)

-- | A decoded backend message. Only the variants the adapter acts on are
-- distinguished; anything else becomes 'UnknownMessage'.
data BackendMessage
  = AuthenticationOk
  | AuthenticationCleartextPassword
  | AuthenticationMD5Password ByteString
  | AuthenticationSASL [ByteString]
  | AuthenticationSASLContinue ByteString
  | AuthenticationSASLFinal ByteString
  | ParameterStatus ByteString ByteString
  | BackendKeyData Int32 Int32
  | ReadyForQuery Word8
  | RowDescription [FieldDescription]
  | DataRow [Maybe ByteString]
  | CommandComplete ByteString
  | EmptyQueryResponse
  | ErrorResponse [(Word8, ByteString)]
  | NoticeResponse [(Word8, ByteString)]
  | ParseComplete
  | BindComplete
  | CloseComplete
  | ParameterDescription [Word32]
  | NoData
  | PortalSuspended
  | NotificationResponse Int32 ByteString ByteString
  | CopyInResponse Int16 [Int16]
  | CopyOutResponse Int16 [Int16]
  | CopyData ByteString
  | CopyDone
  | FunctionCallResponse (Maybe ByteString)
  | UnknownMessage Word8 ByteString
  deriving stock (Eq, Show)

-- | Decode a backend message from its type byte and body.
decodeBackendMessage :: Word8 -> ByteString -> Either DecodingError BackendMessage
decodeBackendMessage typeByte body =
  case typeByte of
    0x52 -> runDecoder authentication body
    0x53 -> runDecoder (ParameterStatus <$> cstring <*> cstring) body
    0x4b -> runDecoder (BackendKeyData <$> int32 <*> int32) body
    0x5a -> runDecoder (ReadyForQuery <$> word8) body
    0x54 -> runDecoder (RowDescription <$> repeatedInt16 fieldDescription) body
    0x44 -> runDecoder (DataRow <$> repeatedInt16 columnValue) body
    0x43 -> runDecoder (CommandComplete <$> cstring) body
    0x49 -> Right EmptyQueryResponse
    0x45 -> runDecoder (ErrorResponse <$> noticeFields) body
    0x4e -> runDecoder (NoticeResponse <$> noticeFields) body
    0x31 -> Right ParseComplete
    0x32 -> Right BindComplete
    0x33 -> Right CloseComplete
    0x74 -> runDecoder (ParameterDescription <$> repeatedInt16 word32) body
    0x6e -> Right NoData
    0x73 -> Right PortalSuspended
    0x41 -> runDecoder (NotificationResponse <$> int32 <*> cstring <*> cstring) body
    0x47 -> runDecoder (uncurry CopyInResponse <$> copyResponse) body
    0x48 -> runDecoder (uncurry CopyOutResponse <$> copyResponse) body
    0x64 -> runDecoder (CopyData <$> remaining) body
    0x63 -> Right CopyDone
    0x56 -> runDecoder (FunctionCallResponse <$> columnValue) body
    other -> Right (UnknownMessage other body)

-- | Repeat a decoder an @Int16@-prefixed number of times.
repeatedInt16 :: Decoder a -> Decoder [a]
repeatedInt16 element = do
  count <- int16
  replicateM (fromIntegral count) element

fieldDescription :: Decoder FieldDescription
fieldDescription =
  FieldDescription
    <$> cstring
    <*> word32
    <*> int16
    <*> word32
    <*> int16
    <*> int32
    <*> int16

columnValue :: Decoder (Maybe ByteString)
columnValue = do
  len <- int32
  if len < 0
    then pure Nothing
    else Just <$> bytes (fromIntegral len)

-- | The fields of an @ErrorResponse@\/@NoticeResponse@: @(code, value)@ pairs,
-- terminated by a zero code byte.
noticeFields :: Decoder [(Word8, ByteString)]
noticeFields = do
  code <- word8
  if code == 0
    then pure []
    else do
      value <- cstring
      ((code, value) :) <$> noticeFields

copyResponse :: Decoder (Int16, [Int16])
copyResponse = do
  overall <- fromIntegral <$> word8
  columns <- repeatedInt16 int16
  pure (overall, columns)

authentication :: Decoder BackendMessage
authentication = do
  subtype <- int32
  case subtype of
    0 -> pure AuthenticationOk
    3 -> pure AuthenticationCleartextPassword
    5 -> AuthenticationMD5Password <$> bytes 4
    10 -> AuthenticationSASL <$> saslMechanisms
    11 -> AuthenticationSASLContinue <$> remaining
    12 -> AuthenticationSASLFinal <$> remaining
    other -> throwE (UnexpectedAuthType other)
  where
    saslMechanisms = do
      mechanism <- cstring
      if ByteString.null mechanism
        then pure []
        else (mechanism :) <$> saslMechanisms
