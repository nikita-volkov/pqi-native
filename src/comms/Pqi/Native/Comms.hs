-- | The serialization abstraction shared by the wire-protocol messages: a type
-- that can be both written (via @ptr-poker@) and decoded (via @ptr-peeker@,
-- with semantic failures carried in an 'ExceptT' layer, since peekers
-- themselves have no failure mechanism).
module Pqi.Native.Comms
  ( Comms (..),
    Decoder,
    DecodingError (..),
    runDecoder,

    -- * Decoder primitives
    liftVariable,
    liftFixed,
    int16,
    int32,
    word8,
    word32,
    cstring,
    bytes,
    remaining,

    -- * Serializable primitives
    CString (..),
  )
where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.ByteString (ByteString)
import Data.Int (Int16, Int32)
import Data.Word (Word32, Word8)
import qualified PtrPeeker as Peeker
import qualified PtrPoker.Write as Poker
import Prelude

-- | A semantic decoding failure (as opposed to a \"need more bytes\" framing
-- shortfall, which the transport handles separately).
data DecodingError
  = -- | A backend message had an unrecognized type byte.
    UnexpectedMessageType Word8
  | -- | An @Authentication@ message had an unrecognized sub-type.
    UnexpectedAuthType Int32
  | -- | A field could not be interpreted (context, offending tag).
    BadField ByteString
  | -- | The message body was shorter than expected.
    TruncatedInput
  | -- | A general protocol violation.
    ProtocolViolation ByteString
  deriving stock (Eq, Show)

-- | A decoder of a message body: a @ptr-peeker@ 'Peeker.Variable' parser with
-- semantic failures layered on top.
type Decoder = ExceptT DecodingError Peeker.Variable

-- | A type that can be serialized to the wire and decoded back from it.
class Comms a where
  toWrite :: a -> Poker.Write
  decoderOf :: Decoder a

-- | Run a decoder against a complete message body. A framing shortfall (the
-- body being shorter than the decoder consumes) is reported as 'TruncatedInput'.
runDecoder :: Decoder a -> ByteString -> Either DecodingError a
runDecoder decoder body =
  case Peeker.runVariableOnByteString (runExceptT decoder) body of
    Left _needed -> Left TruncatedInput
    Right (Left err) -> Left err
    Right (Right value) -> Right value

liftVariable :: Peeker.Variable a -> Decoder a
liftVariable = lift

liftFixed :: Peeker.Fixed a -> Decoder a
liftFixed = lift . Peeker.fixed

int16 :: Decoder Int16
int16 = liftFixed Peeker.beSignedInt2

int32 :: Decoder Int32
int32 = liftFixed Peeker.beSignedInt4

word8 :: Decoder Word8
word8 = liftFixed Peeker.unsignedInt1

word32 :: Decoder Word32
word32 = liftFixed Peeker.beUnsignedInt4

-- | A null-terminated string.
cstring :: Decoder ByteString
cstring = liftVariable Peeker.nullTerminatedStringAsByteString

-- | Exactly @n@ bytes.
bytes :: Int -> Decoder ByteString
bytes n = liftFixed (Peeker.byteArrayAsByteString n)

-- | All remaining bytes of the body.
remaining :: Decoder ByteString
remaining = liftVariable Peeker.remainderAsByteString

-- | A null-terminated string, as a 'Comms' building block.
newtype CString = CString ByteString
  deriving stock (Eq, Show)

instance Comms CString where
  toWrite (CString value) = Poker.byteString value <> Poker.word8 0
  decoderOf = CString <$> cstring

instance Comms Int16 where
  toWrite = Poker.bInt16
  decoderOf = int16

instance Comms Int32 where
  toWrite = Poker.bInt32
  decoderOf = int32

instance Comms Word32 where
  toWrite = Poker.bWord32
  decoderOf = word32

instance Comms Word8 where
  toWrite = Poker.word8
  decoderOf = word8
