-- | The large-object interface, implemented over the server's @lo_*@ SQL
-- functions (rather than libpq's fast-path protocol) — identical results, far
-- less machinery. As with libpq, the open\/read\/write\/close operations must be
-- run inside a transaction managed by the caller.
module Pqi.Native.LargeObject
  ( loCreat,
    loCreate,
    loImport,
    loImportWithOid,
    loExport,
    loOpen,
    loWrite,
    loRead,
    loSeek,
    loTell,
    loTruncate,
    loClose,
    loUnlink,
  )
where

import qualified Data.ByteString.Char8 as ByteString.Char8
import Pqi (Format (..), LoFd (..))
import Pqi.Native.Connection (Connection)
import Pqi.Native.Prelude
import qualified Pqi.Native.Query as Query
import Pqi.Native.Types (NativeResult (..))
import System.IO (IOMode (..), SeekMode (..))

loCreat :: Connection -> IO (Maybe Word32)
loCreat connection =
  (>>= parseOid) <$> callText connection "select lo_creat($1 :: integer)" [intParam (-1)]

loCreate :: Connection -> Word32 -> IO (Maybe Word32)
loCreate connection oid =
  (>>= parseOid) <$> callText connection "select lo_create($1 :: oid)" [oidParam oid]

loImport :: Connection -> FilePath -> IO (Maybe Word32)
loImport connection path =
  (>>= parseOid) <$> callText connection "select lo_import($1)" [textParam (ByteString.Char8.pack path)]

loImportWithOid :: Connection -> FilePath -> Word32 -> IO (Maybe Word32)
loImportWithOid connection path oid =
  (>>= parseOid)
    <$> callText connection "select lo_import($1, $2 :: oid)" [textParam (ByteString.Char8.pack path), oidParam oid]

loExport :: Connection -> Word32 -> FilePath -> IO (Maybe ())
loExport connection oid path =
  succeeded <$> callText connection "select lo_export($1 :: oid, $2)" [oidParam oid, textParam (ByteString.Char8.pack path)]

loOpen :: Connection -> Word32 -> IOMode -> IO (Maybe LoFd)
loOpen connection oid mode =
  fmap (LoFd . fromIntegral)
    . (>>= parseInt)
    <$> callText connection "select lo_open($1 :: oid, $2 :: integer)" [oidParam oid, intParam (ioModeFlag mode)]

loWrite :: Connection -> LoFd -> ByteString -> IO (Maybe Int)
loWrite connection (LoFd fd) payload =
  (>>= parseInt) <$> callText connection "select lowrite($1 :: integer, $2 :: bytea)" [intParam (fromIntegral fd), byteaParam payload]

loRead :: Connection -> LoFd -> Int -> IO (Maybe ByteString)
loRead connection (LoFd fd) len =
  callBinary connection "select loread($1 :: integer, $2 :: integer)" [intParam (fromIntegral fd), intParam len]

loSeek :: Connection -> LoFd -> SeekMode -> Int -> IO (Maybe Int)
loSeek connection (LoFd fd) whence offset =
  (>>= parseInt)
    <$> callText connection "select lo_lseek($1 :: integer, $2 :: integer, $3 :: integer)" [intParam (fromIntegral fd), intParam offset, intParam (seekFlag whence)]

loTell :: Connection -> LoFd -> IO (Maybe Int)
loTell connection (LoFd fd) =
  (>>= parseInt) <$> callText connection "select lo_tell($1 :: integer)" [intParam (fromIntegral fd)]

loTruncate :: Connection -> LoFd -> Int -> IO (Maybe ())
loTruncate connection (LoFd fd) len =
  succeeded <$> callText connection "select lo_truncate($1 :: integer, $2 :: integer)" [intParam (fromIntegral fd), intParam len]

loClose :: Connection -> LoFd -> IO (Maybe ())
loClose connection (LoFd fd) =
  succeeded <$> callText connection "select lo_close($1 :: integer)" [intParam (fromIntegral fd)]

loUnlink :: Connection -> Word32 -> IO (Maybe ())
loUnlink connection oid =
  succeeded <$> callText connection "select lo_unlink($1 :: oid)" [oidParam oid]

-- * Helpers

callText :: Connection -> ByteString -> [Maybe (Word32, ByteString, Format)] -> IO (Maybe ByteString)
callText connection sql params = (>>= firstValue) <$> Query.execParams connection sql params Text

callBinary :: Connection -> ByteString -> [Maybe (Word32, ByteString, Format)] -> IO (Maybe ByteString)
callBinary connection sql params = (>>= firstValue) <$> Query.execParams connection sql params Binary

firstValue :: NativeResult -> Maybe ByteString
firstValue result = case result.rows of
  (cell : _) : _ -> cell
  _ -> Nothing

succeeded :: Maybe ByteString -> Maybe ()
succeeded = fmap (const ())

intParam :: Int -> Maybe (Word32, ByteString, Format)
intParam value = Just (0, ByteString.Char8.pack (show value), Text)

oidParam :: Word32 -> Maybe (Word32, ByteString, Format)
oidParam value = Just (0, ByteString.Char8.pack (show value), Text)

textParam :: ByteString -> Maybe (Word32, ByteString, Format)
textParam value = Just (0, value, Text)

byteaParam :: ByteString -> Maybe (Word32, ByteString, Format)
byteaParam value = Just (17, value, Binary)

parseInt :: ByteString -> Maybe Int
parseInt = fmap fst . ByteString.Char8.readInt

parseOid :: ByteString -> Maybe Word32
parseOid = fmap (fromIntegral . fst) . ByteString.Char8.readInt

ioModeFlag :: IOMode -> Int
ioModeFlag = \case
  ReadMode -> 0x40000 -- INV_READ
  WriteMode -> 0x20000 -- INV_WRITE
  AppendMode -> 0x20000
  ReadWriteMode -> 0x60000 -- INV_READ | INV_WRITE

seekFlag :: SeekMode -> Int
seekFlag = \case
  AbsoluteSeek -> 0
  RelativeSeek -> 1
  SeekFromEnd -> 2
