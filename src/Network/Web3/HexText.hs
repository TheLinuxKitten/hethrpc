{-# LANGUAGE FlexibleInstances #-}

--------------------------------------------------------------------------
--
-- Copyright: (c) Javier López Durá
-- License: BSD3
--
-- Maintainer: Javier López Durá <linux.kitten@gmail.com>
--
--------------------------------------------------------------------------

module Network.Web3.HexText
  ( HexStr
  , HexText(..)
  , joinHex
  , stripHex
  , hex2bs
  , bs2hex
  , hexStrToBs
  , bsToHexStr 
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Char as C
import Data.Int
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Data.Word

-- | Este tipo se usa para almacenar cadenas hexadecimales formateadas según la especificación JSON-RPC de Ethereum.
type HexStr = T.Text

-- | Clase para convertir datos a/desde cadenas hexadecimales JSON-RPC ethereum.
class HexText a where
  toHex :: a -> HexStr
  fromHex :: HexStr -> a

instance HexText Bool where
  toHex = toHex . fromEnum
  fromHex = toEnum . fromHex

instance HexText Integer where
  toHex = integer2Hex
  fromHex = hex2Integer

instance HexText Int where
  toHex = toHex . toInteger
  fromHex = fromInteger . fromHex

instance HexText Int8 where
  toHex = toHex . toInteger
  fromHex = fromInteger . fromHex

instance HexText Int16 where
  toHex = toHex . toInteger
  fromHex = fromInteger . fromHex

instance HexText Int32 where
  toHex = toHex . toInteger
  fromHex = fromInteger . fromHex

instance HexText Int64 where
  toHex = toHex . toInteger
  fromHex = fromInteger . fromHex

instance HexText Word where
  toHex = toHex . toInteger
  fromHex = fromInteger . fromHex

instance HexText Word8 where
  toHex = toHex . toInteger
  fromHex = fromInteger . fromHex

instance HexText Word16 where
  toHex = toHex . toInteger
  fromHex = fromInteger . fromHex

instance HexText Word32 where
  toHex = toHex . toInteger
  fromHex = fromInteger . fromHex

instance HexText Word64 where
  toHex = toHex . toInteger
  fromHex = fromInteger . fromHex

instance HexText BS.ByteString where
  toHex = bsToHexStr
  fromHex = hexStrToBs

instance HexText LBS.ByteString where
  toHex = toHex . LBS.toStrict
  fromHex = LBS.fromStrict . fromHex

instance HexText String where
  toHex = toHex . C8.pack
  fromHex = C8.unpack . fromHex

instance HexText T.Text where
  toHex = toHex . TE.encodeUtf8
  fromHex = TE.decodeUtf8 . fromHex

instance HexText TL.Text where
  toHex = toHex . TL.toStrict
  fromHex = TL.fromStrict . fromHex

stripHex = T.drop 2
joinHex = T.append (T.pack "0x")

hex2Integer :: HexStr -> Integer
hex2Integer = fst . T.foldr addF (0,0) . stripHex
  where
    addF c (r,i) = ((toInteger (C.digitToInt c)*(16^i))+r,i+1)

integer2Hex :: Integer -> HexStr
integer2Hex = joinHex . int2Text T.empty
  where
    int2Text res i
      | i == 0 = if res == T.empty then T.pack "0" else res
      | otherwise =
        let (c,r) = divMod i 16
        in int2Text (T.cons (C.intToDigit $ fromInteger r) res) c

-- | Decodifica cadena hexadecimal (sin prefijo "0x")
hex2bs :: T.Text -> BS.ByteString
hex2bs = fst . BS16.decode . TE.encodeUtf8

-- | Codifica a cadena hexadecimal (sin prefijo "0x")
bs2hex :: BS.ByteString -> T.Text
bs2hex = TE.decodeUtf8 . BS16.encode

-- | Decodifica cadena hexadecimal (con prefijo "0x")
hexStrToBs :: HexStr -> BS.ByteString
hexStrToBs = hex2bs . stripHex

-- | Codifica a cadena hexadecimal (con prefijo "0x")
bsToHexStr :: BS.ByteString -> HexStr
bsToHexStr = joinHex . bs2hex

