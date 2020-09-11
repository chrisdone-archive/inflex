{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveGeneric #-}

-- | SHA256 digest/hashing.

module Inflex.Server.Types.Sha256
  ( sha256
  , sha256HexParser
  , sha256ByteString
  , sha256String
  , sha256AsHexText
  , sha256AsHexBS
  , checkSha256Of
  , valueToSha256
  , sha256Text
  , Sha256(..)
  ) where

import qualified Crypto.Hash as Hash (Digest, SHA256, hash)
import           Data.Aeson
import qualified Data.Attoparsec.Text as Atto.T
import           Data.ByteArray
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Base16 as Hex
import           Data.ByteString.Lazy (toStrict)
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Encoding as T
import           Database.Persist.Sql
import           GHC.Generics
import           Language.Haskell.TH
import           Language.Haskell.TH.Instances ()
import           Language.Haskell.TH.Syntax (Lift(..), Q, TExp(..))

--------------------------------------------------------------------------------
-- Type

-- | A Sha256 key to address blobs.
newtype Sha256 =
  Sha256 ByteString
  deriving (Eq, Ord, Lift, Generic, PersistFieldSql, PersistField)

--------------------------------------------------------------------------------
-- Instances

instance Show Sha256 where
  show (Sha256 key) = show (Hex.encode key)

--------------------------------------------------------------------------------
-- JSON

instance ToJSON Sha256 where
  toJSON sha = String (sha256AsHexText sha)

instance ToJSONKey Sha256

instance FromJSON Sha256 where
  parseJSON (String val) = pure $ Sha256 $ fst $ Hex.decode $ T.encodeUtf8 val
  parseJSON value =
    fail $ "Expected JSON value of String, but instead got " <> show value

instance FromJSONKey Sha256

--------------------------------------------------------------------------------
-- Parsing

-- | Parse a blob key in hex format.
sha256HexParser :: Text -> Either String Sha256
sha256HexParser =
  Atto.T.parseOnly
    (fmap
       Sha256
       (do bytes <- Atto.T.take 64
           case Hex.decode (T.encodeUtf8 bytes) of
             (result, wrong)
               | S.null wrong -> pure result
             _ -> fail "Invalid hex key."))

--------------------------------------------------------------------------------
-- Template Haskell

sha256 :: Text -> Q Exp
sha256 i =
  case sha256HexParser i of
    Left e -> error e
    Right v -> lift v

instance IsString (Q (TExp Sha256)) where
  fromString i =
    if Prelude.length i == 64
      then case Hex.decode (fromString i) of
             (result, wrong)
               | S.null wrong -> fmap TExp (lift (Sha256 result))
             _ -> fail "Invalid Sha256 format."
      else fail "Incorrect length for Sha256."

--------------------------------------------------------------------------------
-- Representations

sha256AsHexText :: Sha256 -> Text
sha256AsHexText = decodeUtf8 . sha256AsHexBS

sha256AsHexBS :: Sha256 -> ByteString
sha256AsHexBS (Sha256 key) = Hex.encode key

--------------------------------------------------------------------------------
-- Hasing things

valueToSha256 :: Value -> Sha256
valueToSha256 value = sha256ByteString $ toStrict $ encode value

sha256ByteString :: ByteString -> Sha256
sha256ByteString =
  Sha256 . convert . (Hash.hash :: ByteString -> Hash.Digest Hash.SHA256)

checkSha256Of :: Sha256 -> ByteString -> Bool
checkSha256Of hash bs = hash == sha256ByteString bs

sha256String :: String -> Sha256
sha256String str = sha256ByteString $ encodeUtf8 $ T.pack str

sha256Text :: Text -> Sha256
sha256Text str = sha256ByteString $ encodeUtf8 str
