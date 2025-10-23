{-# LANGUAGE OverloadedStrings #-}

{-|
Convenience wrappers around Aeson for cleaner JSON handling.
Includes pretty printing for debugging.
-}

module Utils.JSON
    ( -- * Encoding
      encodeJSON
    , encodePrettyJSON
    
    -- * Decoding
    , decodeJSON
    , decodeJSONStrict
    
    -- * Pretty printing
    , prettyPrintJSON
    , prettyPrintValue
    
    -- * Helpers
    , toJSONText
    , fromJSONText
    ) where

import Data.Aeson (ToJSON, FromJSON, encode, decode, eitherDecode)
import Data.Aeson.Encode.Pretty (encodePretty, Config(..), defConfig)
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

-- ============================================================================
-- Encoding
-- ============================================================================

encodeJSON :: ToJSON a => a -> BL.ByteString
encodeJSON = encode
{-# INLINE encodeJSON #-}

encodePrettyJSON :: ToJSON a => a -> BL.ByteString
encodePrettyJSON = encodePretty' prettyConfig
  where
    prettyConfig = defConfig
        { confIndent = 2  -- 2-space indentation
        , confCompare = compare  -- Sort keys alphabetically
        }

-- ============================================================================
-- Decoding
-- ============================================================================

decodeJSON :: FromJSON a => BL.ByteString -> Maybe a
decodeJSON = decode
{-# INLINE decodeJSON #-}

decodeJSONStrict :: FromJSON a => BL.ByteString -> Either String a
decodeJSONStrict = eitherDecode
{-# INLINE decodeJSONStrict #-}

-- ============================================================================
-- Pretty Printing (for debugging)
-- ============================================================================

prettyPrintJSON :: ToJSON a => a -> IO ()
prettyPrintJSON val = do
    let pretty = encodePrettyJSON val
    BL8.putStrLn pretty  -- đổi BL -> BL8

prettyPrintValue :: Aeson.Value -> IO ()
prettyPrintValue val = do
    let pretty = encodePretty val  -- dùng encodePretty trực tiếp
    BL8.putStrLn pretty

-- ============================================================================
-- Text Helpers
-- ============================================================================

toJSONText :: ToJSON a => a -> TL.Text
toJSONText = TLE.decodeUtf8 . encode

fromJSONText :: FromJSON a => TL.Text -> Maybe a
fromJSONText = decode . TLE.encodeUtf8

encodeJSONStrict :: ToJSON a => a -> BS.ByteString
encodeJSONStrict = BL.toStrict . encode

decodeJSONStrictBS :: FromJSON a => BS.ByteString -> Maybe a
decodeJSONStrictBS = decode . BL.fromStrict
