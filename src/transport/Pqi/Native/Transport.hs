-- | The byte-level transport: a TCP socket with a read buffer, plus the framing
-- that turns the stream into discrete @[type byte][Int32 length][body]@
-- messages.
module Pqi.Native.Transport
  ( Transport,
    connect,
    isUnixSocketHost,
    unixSocketPath,
    unconnected,
    close,
    send,
    receiveFrame,
    socketFd,
    peerIp,
    readUntilClosed,
  )
where

import Control.Exception (mask_)
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

-- | Open a connection to the given host and port. Mirroring libpq: if the
-- host looks like an absolute path (starts with @\/@ - see 'isUnixSocketHost'),
-- it names a Unix-domain socket /directory/ rather than a TCP host, and the
-- connection is made to the socket file @'unixSocketPath' host port@ within
-- it. Otherwise a TCP connection is made, resolving the host via DNS.
connect :: ByteString -> Int -> IO Transport
connect host port
  | isUnixSocketHost host = connectUnix (unixSocketPath host port)
  | otherwise = connectTcp host port

-- | Whether a conninfo @host@ value names a Unix-domain socket directory
-- rather than a TCP host - i.e. it looks like an absolute path, per libpq's
-- rule: "If a host name looks like an absolute path name, it specifies
-- Unix-domain communication rather than TCP/IP communication".
isUnixSocketHost :: ByteString -> Bool
isUnixSocketHost host = not (ByteString.null host) && ByteString.head host == 0x2f -- '/'

-- | The path of the socket file libpq expects within a Unix-domain socket
-- directory: @\<directory\>\/.s.PGSQL.\<port\>@.
unixSocketPath :: ByteString -> Int -> FilePath
unixSocketPath directory port = ByteString.Char8.unpack directory <> "/.s.PGSQL." <> show port

-- | Open a TCP connection to the given host and port.
connectTcp :: ByteString -> Int -> IO Transport
connectTcp host port = do
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

-- | Open a connection to a Unix-domain socket at the given path (the
-- directory\/@\.s\.PGSQL\.\<port\>@ file, per 'unixSocketPath').
connectUnix :: FilePath -> IO Transport
connectUnix path
  | not Socket.isUnixDomainSocketAvailable =
      ioError (userError "pqi-native: Unix-domain sockets are not supported on this platform")
  | otherwise = do
      sock <- Socket.socket Socket.AF_UNIX Socket.Stream 0
      Socket.connect sock (Socket.SockAddrUnix path)
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
close transport = Socket.close (socket transport)

-- | The underlying socket file descriptor.
socketFd :: Transport -> IO Int32
socketFd transport = fromIntegral <$> Socket.unsafeFdSocket (socket transport)

-- | Send a serialized message.
send :: Transport -> Poker.Write -> IO ()
send transport write = Socket.ByteString.sendAll (socket transport) (Poker.toByteString write)

-- | Ensure the read buffer holds at least @n@ bytes, pulling from the socket
-- as needed. Throws on EOF before @n@ bytes are available.
--
-- The wait for bytes is deliberately left interruptible. Nothing has been
-- consumed at this point, so a caller that gives up here loses nothing - and
-- is /able/ to give up. A caller blocked on a message the server
-- will never send (an aborted pipeline whose bookkeeping has drifted, say)
-- must stay abandonable by 'System.Timeout.timeout'; masking the wait
-- uninterruptibly turns that stall into a deadlock no timer can break.
--
-- Only the step that moves bytes off the socket and into the buffer is masked,
-- which is all the atomicity the framing needs: an async exception can never
-- land in the gap between @recv@ returning and its bytes being recorded, so
-- bytes are never dropped. @recv@ itself stays interruptible inside 'mask_',
-- so the blocking wait keeps its abandonability.
fillTo :: Transport -> Int -> IO ()
fillTo transport n = go
  where
    go = do
      buffered <- readIORef (readBuffer transport)
      let missing = n - ByteString.length buffered
      when (missing > 0) do
        closed <- mask_ do
          chunk <- Socket.ByteString.recv (socket transport) (max 4096 missing)
          if ByteString.null chunk
            then pure True
            else do
              modifyIORef' (readBuffer transport) (<> chunk)
              pure False
        when closed do
          ioError (mkIOError eofErrorType "pqi-native: connection closed by server" Nothing Nothing)
        go

-- | Receive one framed message: its type byte and its body (the length prefix,
-- which counts itself, is consumed).
--
-- Buffering the whole frame before consuming any of it keeps the framing
-- atomic without masking the wait: an async exception landing while the frame
-- is still incomplete leaves the buffer untouched, and the frame is taken out
-- of the buffer in a single 'atomicModifyIORef'' step. The earlier shape -
-- consuming the header, then blocking again for the body - is what made a
-- mid-read interrupt desync the connection, and what an outer
-- 'Control.Exception.uninterruptibleMask_' was papering over at the cost of
-- making every stall permanent.
receiveFrame :: Transport -> IO (Word8, ByteString)
receiveFrame transport = do
  fillTo transport 5
  header <- ByteString.take 5 <$> readIORef (readBuffer transport)
  let frameLength = decodeInt32BE (ByteString.drop 1 header)
      frameSize = 5 + max 0 (frameLength - 4)
  fillTo transport frameSize
  atomicModifyIORef' (readBuffer transport) \buffered ->
    let (frame, rest) = ByteString.splitAt frameSize buffered
     in (rest, (ByteString.head frame, ByteString.drop 5 frame))

-- | Read bytes until the peer closes the connection (EOF), discarding them.
-- Used by the cancel path to mirror libpq's behaviour: keep the socket open
-- until the server has read the cancel request and closed its end.
readUntilClosed :: Transport -> IO ()
readUntilClosed transport = go
  where
    go = do
      chunk <- Socket.ByteString.recv (socket transport) 4096
      if ByteString.null chunk
        then pure ()
        else go

-- | The numeric IP address of the connected peer (e.g. @\"::1\"@ or
-- @\"127.0.0.1\"@). Throws if the socket has no peer (unconnected).
peerIp :: Transport -> IO ByteString
peerIp transport = do
  addr <- Socket.getPeerName (socket transport)
  (Just ip, _) <- Socket.getNameInfo [Socket.NI_NUMERICHOST] True False addr
  pure (ByteString.Char8.pack ip)

decodeInt32BE :: ByteString -> Int
decodeInt32BE = ByteString.foldl' (\acc w -> acc * 256 + fromIntegral w) 0 . ByteString.take 4
