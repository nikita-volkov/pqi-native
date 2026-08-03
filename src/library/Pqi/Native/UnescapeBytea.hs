-- | Pure implementation of @bytea@ unescaping. See 'unescapeBytea'.
module Pqi.Native.UnescapeBytea
  ( unescapeBytea,
  )
where

import Data.ByteString (ByteString)
import Data.Either (fromRight)
import Data.Word (Word8)
import Prelude
import PtrPeeker (Variable, fixed, hasMore, runVariableOnByteString, unsignedInt1)
import PtrPoker.Write (Write)
import qualified PtrPoker.Write as Write

-- | Convert the textual representation of a @bytea@ value, as produced by
-- the server, back into raw bytes. Both the modern @\\x@ hex format (lowercase
-- @x@ only) and the legacy escape format are accepted.
--
-- Malformed input is tolerated exactly the way @PQunescapeBytea@ tolerates it:
-- in hex format, characters that are not hex digits (including whitespace) are
-- silently skipped, and a hex digit whose pair character is invalid is
-- dropped; in escape format, an invalid escape simply drops the backslash, and
-- an octal escape must start with @0@..@3@. Input is treated as a C string:
-- the first NUL byte terminates processing.
unescapeBytea :: ByteString -> ByteString
unescapeBytea input =
  Write.toByteString
    $ fromRight mempty
    $ runVariableOnByteString decoder input

-- Inline NUL truncation and \x prefix detection so no intermediate ByteStrings
-- are allocated before dispatching to the format-specific decoder.
decoder :: Variable Write
decoder = do
  more <- hasMore
  if not more
    then return mempty
    else do
      b0 <- fixed unsignedInt1
      case b0 of
        0x00 -> return mempty -- NUL: C-string terminator
        0x5c -> do
          -- backslash: probe for the \x hex-format prefix
          more2 <- hasMore
          if not more2
            then return mempty -- single trailing backslash
            else do
              b1 <- fixed unsignedInt1
              if b1 == 0x78 -- lowercase 'x': enter hex mode
                then hexDecoder
                else afterBackslash b1 -- escape mode; b1 follows the consumed '\'
        _ -> (Write.word8 b0 <>) <$> escapeDecoder

-- | Hex-format decoder. Skips non-hex bytes (matching @PQunescapeBytea@),
-- pairs hex nibbles, and stops at a NUL byte (C-string terminator).
hexDecoder :: Variable Write
hexDecoder = do
  more <- hasMore
  if not more
    then return mempty
    else do
      a <- fixed unsignedInt1
      if a == 0x00
        then return mempty -- NUL: stop
        else case hexValue a of
          Nothing -> hexDecoder -- skip non-hex byte
          Just hi -> do
            more2 <- hasMore
            if not more2
              then return mempty -- drop unpaired nibble
              else do
                b <- fixed unsignedInt1
                if b == 0x00
                  then return mempty -- NUL: stop, drop unpaired nibble
                  else case hexValue b of
                    Nothing -> hexDecoder -- skip b, look for next pair
                    Just lo -> (Write.word8 (hi * 16 + lo) <>) <$> hexDecoder
  where
    hexValue :: Word8 -> Maybe Word8
    hexValue w
      | w >= 0x30 && w <= 0x39 = Just (w - 0x30)
      | w >= 0x61 && w <= 0x66 = Just (w - 0x57)
      | w >= 0x41 && w <= 0x46 = Just (w - 0x37)
      | otherwise = Nothing

-- | Escape-format decoder. Processes bytes as escape sequences and stops at
-- a NUL byte (C-string terminator).
escapeDecoder :: Variable Write
escapeDecoder = do
  more <- hasMore
  if not more
    then return mempty
    else do
      b <- fixed unsignedInt1
      case b of
        0x00 -> return mempty
        0x5c -> handleEscapeBackslash
        _ -> (Write.word8 b <>) <$> escapeDecoder

-- | Handle the bytes that follow a consumed backslash in escape format.
-- Exported so the top-level dispatcher can reuse it after consuming the
-- @\\x@ prefix check.
afterBackslash :: Word8 -> Variable Write
afterBackslash next = case next of
  0x00 -> return mempty
  0x5c -> (Write.word8 0x5c <>) <$> escapeDecoder
  _ -> octalOrLiteralDecoder next

handleEscapeBackslash :: Variable Write
handleEscapeBackslash = do
  more <- hasMore
  if not more
    then return mempty -- trailing backslash: drop it
    else do
      next <- fixed unsignedInt1
      afterBackslash next

-- | Try to decode a 3-digit octal starting with @a@ (already consumed).
-- Falls back to emitting @a@ literally and re-routing the consumed lookahead
-- byte(s) through 'afterEscape', reproducing @PQunescapeBytea@'s backtracking.
octalOrLiteralDecoder :: Word8 -> Variable Write
octalOrLiteralDecoder a
  | isFirstOctal a = do
      more <- hasMore
      if not more
        then return (Write.word8 a)
        else do
          b <- fixed unsignedInt1
          if not (isOctal b)
            then (Write.word8 a <>) <$> afterEscape b -- b isn't octal: emit a, re-route b
            else do
              more2 <- hasMore
              if not more2
                then return (Write.word8 a <> Write.word8 b) -- only two digits: both literal
                else do
                  c <- fixed unsignedInt1
                  if isOctal c
                    then (Write.word8 (octal a b c) <>) <$> escapeDecoder
                    else (\x -> Write.word8 a <> Write.word8 b <> x) <$> afterEscape c
  | otherwise = (Write.word8 a <>) <$> escapeDecoder
  where
    -- \| Route an already-consumed byte back through the escape-format main loop.
    -- Used when a consumed lookahead byte must be re-processed after a failed
    -- octal-triple attempt.
    afterEscape :: Word8 -> Variable Write
    afterEscape b = case b of
      0x00 -> return mempty
      0x5c -> handleEscapeBackslash
      _ -> (Write.word8 b <>) <$> escapeDecoder

    isFirstOctal :: Word8 -> Bool
    isFirstOctal w = w >= 0x30 && w <= 0x33

    isOctal :: Word8 -> Bool
    isOctal w = w >= 0x30 && w <= 0x37

    octal :: Word8 -> Word8 -> Word8 -> Word8
    octal a b c = (a - 0x30) * 64 + (b - 0x30) * 8 + (c - 0x30)
