{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveGeneric #-}

-- | SHA512 digest/hashing.

module Inflex.Types.SHA512
  ( sha512
  , sha512HexParser
  , sha512ByteString
  , sha512String
  , sha512AsHexText
  , sha512AsHexBS
  , checkSha512Of
  , valueToSha512
  , SHA512(..)
  ) where

import qualified Crypto.Hash as Hash (Digest, SHA512, hash)
import Data.Aeson
import qualified Data.Attoparsec.Text as Atto.T
import Data.ByteArray
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Base16 as Hex
import Data.ByteString.Lazy (toStrict)
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Encoding as T
import GHC.Generics
import Language.Haskell.TH
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Syntax (Lift(..), Q, TExp(..))

--------------------------------------------------------------------------------
-- Type

-- | A SHA512 key to address blobs.
newtype SHA512 =
  SHA512 ByteString
  deriving (Eq, Ord, Lift, Generic)

--------------------------------------------------------------------------------
-- Instances

instance Show SHA512 where
  show (SHA512 key) = show (Hex.encode key)

--------------------------------------------------------------------------------
-- JSON

instance ToJSON SHA512 where
  toJSON sha = String (sha512AsHexText sha)

instance ToJSONKey SHA512

instance FromJSON SHA512 where
  parseJSON (String val) = pure $ SHA512 $ fst $ Hex.decode $ T.encodeUtf8 val
  parseJSON value =
    error $ "Expected JSON value of String, but instead got " <> show value

instance FromJSONKey SHA512

--------------------------------------------------------------------------------
-- Parsing

-- | Parse a blob key in hex format.
sha512HexParser :: Text -> Either String SHA512
sha512HexParser =
  Atto.T.parseOnly
    (fmap
       SHA512
       (do bytes <- Atto.T.take 64
           case Hex.decode (T.encodeUtf8 bytes) of
             (result, wrong)
               | S.null wrong -> pure result
             _ -> fail "Invalid hex key."))

--------------------------------------------------------------------------------
-- Template Haskell

sha512 :: Text -> Q Exp
sha512 i =
  case sha512HexParser i of
    Left e -> error e
    Right v -> lift v

instance IsString (Q (TExp SHA512)) where
  fromString i =
    if Prelude.length i == 64
      then case Hex.decode (fromString i) of
             (result, wrong)
               | S.null wrong -> fmap TExp (lift (SHA512 result))
             _ -> fail "Invalid SHA512 format."
      else fail "Incorrect length for SHA512."

--------------------------------------------------------------------------------
-- Representations

sha512AsHexText :: SHA512 -> Text
sha512AsHexText = decodeUtf8 . sha512AsHexBS

sha512AsHexBS :: SHA512 -> ByteString
sha512AsHexBS (SHA512 key) = Hex.encode key

--------------------------------------------------------------------------------
-- Hasing things

valueToSha512 :: Value -> SHA512
valueToSha512 value = sha512ByteString $ toStrict $ encode value

sha512ByteString :: ByteString -> SHA512
sha512ByteString =
  SHA512 . convert . (Hash.hash :: ByteString -> Hash.Digest Hash.SHA512)

checkSha512Of :: SHA512 -> ByteString -> Bool
checkSha512Of hash bs = hash == sha512ByteString bs

sha512String :: String -> SHA512
sha512String str = sha512ByteString $ encodeUtf8 $ T.pack str
