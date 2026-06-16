-- | The byte-level transport: a TCP socket with a read buffer, plus the framing
-- that turns the stream into discrete @[type byte][Int32 length][body]@
-- messages.
module Pqi.Native.Transport
  ( Transport,
    connect,
    unconnected,
    close,
    send,
    receiveFrame,
    socketFd,
    peerIp,
  )
where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.IORef
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as Socket.ByteString
import Pqi.Native.Transport.Prelude
import qualified PtrPoker.Write as Poker
import System.IO.Error (eofErrorType, mkIOError)

-- | An open connection's byte transport.
data Transport = Transport
  { socket :: Socket.Socket,
    readBuffer :: IORef ByteString
  }

-- | Open a TCP connection to the given host and port.
connect :: ByteString -> Int -> IO Transport
connect host port = do
  let hints = Socket.defaultHints {Socket.addrSocketType = Socket.Stream}
  addresses <-
    Socket.getAddrInfo (Just hints) (Just (ByteString.Char8.unpack host)) (Just (show port))
  case addresses of
    [] -> ioError (userError ("Could not resolve host: " <> ByteString.Char8.unpack host))
    address : _ -> do
      sock <- Socket.socket (Socket.addrFamily address) (Socket.addrSocketType address) (Socket.addrProtocol address)
      Socket.connect sock (Socket.addrAddress address)
      buffer <- newIORef ByteString.empty
      pure Transport {socket = sock, readBuffer = buffer}

-- | An unconnected transport, for representing a \"null\" connection. Its
-- socket is allocated but never connected; it must not be used for I\/O.
unconnected :: IO Transport
unconnected = do
  sock <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
  buffer <- newIORef ByteString.empty
  pure Transport {socket = sock, readBuffer = buffer}

-- | Close the connection.
close :: Transport -> IO ()
close transport = Socket.close transport.socket

-- | The underlying socket file descriptor.
socketFd :: Transport -> IO Int32
socketFd transport = fromIntegral <$> Socket.unsafeFdSocket transport.socket

-- | Send a serialized message.
send :: Transport -> Poker.Write -> IO ()
send transport write = Socket.ByteString.sendAll transport.socket (Poker.toByteString write)

-- | Read exactly @n@ bytes, looping over @recv@ (which yields up to @n@) and
-- buffering any overshoot. Throws on EOF before @n@ bytes arrive.
receiveExactly :: Transport -> Int -> IO ByteString
receiveExactly transport n = do
  buffered <- readIORef transport.readBuffer
  go buffered
  where
    go accumulated
      | ByteString.length accumulated >= n = do
          let (result, rest) = ByteString.splitAt n accumulated
          writeIORef transport.readBuffer rest
          pure result
      | otherwise = do
          chunk <- Socket.ByteString.recv transport.socket (max 4096 (n - ByteString.length accumulated))
          if ByteString.null chunk
            then ioError (mkIOError eofErrorType "pqi-native: connection closed by server" Nothing Nothing)
            else go (accumulated <> chunk)

-- | Receive one framed message: its type byte and its body (the length prefix,
-- which counts itself, is consumed).
receiveFrame :: Transport -> IO (Word8, ByteString)
receiveFrame transport = do
  header <- receiveExactly transport 5
  let typeByte = ByteString.head header
      frameLength = decodeInt32BE (ByteString.drop 1 header)
      bodyLength = frameLength - 4
  body <-
    if bodyLength > 0
      then receiveExactly transport bodyLength
      else pure ByteString.empty
  pure (typeByte, body)

-- | The numeric IP address of the connected peer (e.g. @\"::1\"@ or
-- @\"127.0.0.1\"@). Throws if the socket has no peer (unconnected).
peerIp :: Transport -> IO ByteString
peerIp transport = do
  addr <- Socket.getPeerName transport.socket
  (Just ip, _) <- Socket.getNameInfo [Socket.NI_NUMERICHOST] True False addr
  pure (ByteString.Char8.pack ip)

decodeInt32BE :: ByteString -> Int
decodeInt32BE = ByteString.foldl' (\acc w -> acc * 256 + fromIntegral w) 0 . ByteString.take 4
