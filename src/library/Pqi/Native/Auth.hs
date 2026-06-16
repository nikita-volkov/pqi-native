-- | Authentication helpers: MD5 password hashing and the SASL\/SCRAM-SHA-256
-- exchange.
--
-- The pure crypto lives here; the message round-trip is abstracted as 'SaslStep'
-- so the connection module owns the actual socket I\/O.
module Pqi.Native.Auth
  ( md5Password,
    SaslStep (..),
    SaslMessage (..),
    scram,
  )
where

import Crypto.Hash (Digest, MD5 (..), SHA256 (..), hashWith)
import Crypto.KDF.PBKDF2 (Parameters (..), fastPBKDF2_SHA256)
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import Crypto.Random (getRandomBytes)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.List as List
import Pqi.Native.Prelude

-- | Compute the response to an @AuthenticationMD5Password@ challenge:
-- @"md5" <> md5hex (md5hex (password <> user) <> salt)@.
md5Password :: ByteString -> ByteString -> ByteString -> ByteString
md5Password user password salt =
  "md5" <> md5Hex (md5Hex (password <> user) <> salt)

md5Hex :: ByteString -> ByteString
md5Hex = ByteString.Char8.pack . show . hashWith MD5

-- | One server SASL\/authentication message, as the SCRAM logic sees it.
data SaslMessage
  = SaslContinue ByteString
  | SaslFinal ByteString
  | SaslOk
  | SaslError ByteString

-- | The message round-trip the SCRAM exchange drives, supplied by the
-- connection module.
data SaslStep = SaslStep
  { sendInitial :: ByteString -> ByteString -> IO (),
    sendResponse :: ByteString -> IO (),
    receive :: IO SaslMessage
  }

mechanismName :: ByteString
mechanismName = "SCRAM-SHA-256"

-- | Run the SCRAM-SHA-256 exchange (without channel binding). Returns @Right ()@
-- once the server accepts the client proof, or @Left@ with a problem
-- description.
scram :: ByteString -> ByteString -> [ByteString] -> SaslStep -> IO (Either ByteString ())
scram _user password mechanisms step
  | mechanismName `notElem` mechanisms =
      pure (Left "server did not offer SCRAM-SHA-256")
  | otherwise = do
      clientNonce <- Base64.encode <$> getRandomBytes 18
      let clientFirstBare = "n=,r=" <> clientNonce
      step.sendInitial mechanismName ("n,," <> clientFirstBare)
      step.receive >>= \case
        SaslError problem -> pure (Left problem)
        SaslContinue serverFirst ->
          case parseServerFirst serverFirst of
            Nothing -> pure (Left "malformed SCRAM server-first message")
            Just (serverNonce, salt, iterations) -> do
              let saltedPassword = fastPBKDF2_SHA256 (Parameters iterations 32) password salt :: ByteString
                  clientKey = hmacSha256 saltedPassword "Client Key"
                  storedKey = sha256 clientKey
                  clientFinalWithoutProof = "c=biws,r=" <> serverNonce
                  authMessage =
                    ByteString.intercalate "," [clientFirstBare, serverFirst, clientFinalWithoutProof]
                  clientSignature = hmacSha256 storedKey authMessage
                  clientProof = xorBytes clientKey clientSignature
                  clientFinal = clientFinalWithoutProof <> ",p=" <> Base64.encode clientProof
              step.sendResponse clientFinal
              step.receive >>= \case
                SaslFinal _ -> pure (Right ())
                SaslOk -> pure (Right ())
                SaslError problem -> pure (Left problem)
                _ -> pure (Left "unexpected SCRAM server-final message")
        _ -> pure (Left "unexpected SCRAM message")

-- | Parse @r=<nonce>,s=<salt base64>,i=<iterations>@ into the server nonce, the
-- decoded salt, and the iteration count.
parseServerFirst :: ByteString -> Maybe (ByteString, ByteString, Int)
parseServerFirst message = do
  let attributes = ByteString.Char8.split ',' message
  nonce <- attributeValue "r=" attributes
  saltEncoded <- attributeValue "s=" attributes
  salt <- either (const Nothing) Just (Base64.decode saltEncoded)
  iterationsText <- attributeValue "i=" attributes
  (iterations, _) <- ByteString.Char8.readInt iterationsText
  pure (nonce, salt, iterations)
  where
    attributeValue prefix =
      fmap (ByteString.drop (ByteString.length prefix))
        . List.find (ByteString.isPrefixOf prefix)

hmacSha256 :: ByteString -> ByteString -> ByteString
hmacSha256 key message = digestBytes (hmacGetDigest (hmac key message :: HMAC SHA256))

sha256 :: ByteString -> ByteString
sha256 = digestBytes . hashWith SHA256

-- | Extract the raw bytes of a digest via its hexadecimal 'Show' instance,
-- avoiding a direct @memory@ dependency (whose @ByteArrayAccess@ instance for
-- crypton's @Digest@ conflicts across package versions).
digestBytes :: Digest a -> ByteString
digestBytes = hexToBytes . ByteString.Char8.pack . show

hexToBytes :: ByteString -> ByteString
hexToBytes = ByteString.pack . pairs . ByteString.unpack
  where
    pairs (hi : lo : rest) = (hexValue hi * 16 + hexValue lo) : pairs rest
    pairs _ = []
    hexValue w
      | w >= 0x30 && w <= 0x39 = w - 0x30
      | w >= 0x61 && w <= 0x66 = w - 0x57
      | w >= 0x41 && w <= 0x46 = w - 0x37
      | otherwise = 0

xorBytes :: ByteString -> ByteString -> ByteString
xorBytes a b = ByteString.pack (ByteString.zipWith xor a b)
