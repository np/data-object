{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
---------------------------------------------------------
--
-- Module        : Data.Object.Raw
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
--
-- Objects with bytestrings for keys and values. This also includes some nice
-- utilities for converting to/from these.
--
-- This is especially useful for serializing to/from files like JSON and Yaml.
---------------------------------------------------------
module Data.Object.Raw
    ( Raw
    , RawObject
    , FromRaw (..)
    , ToRaw (..)
    , oLookup
    , module Data.Object
    ) where

import Data.Object
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import Data.ByteString.Class
import Data.Time.Calendar
import Safe (readMay)

type Raw = B.ByteString

type RawObject = Object Raw Raw

class ToRaw a where
    toRaw :: a -> B.ByteString
class FromRaw a where
    fromRaw :: MonadFail m => B.ByteString -> m a

instance ToRaw Raw where
    toRaw = id
instance FromRaw Raw where
    fromRaw = return
instance ToObject Raw key Raw where
    toObject = Scalar
instance FromObject Raw key Raw where
    fromObject = getScalar

instance ToRaw BS.ByteString where
    toRaw = toLazyByteString
instance FromRaw BS.ByteString where
    fromRaw = return . fromLazyByteString
instance ToObject BS.ByteString key Raw where
    toObject = Scalar . toRaw
instance FromObject BS.ByteString key Raw where
    fromObject o = fromObject o >>= fromRaw

instance ToRaw String where
    toRaw = toLazyByteString
instance FromRaw String where
    fromRaw = return . fromLazyByteString
instance ToObject String key Raw where
    toObject = Scalar . toRaw
instance FromObject String key Raw where
    fromObject o = fromObject o >>= fromRaw

oLookup :: (MonadFail m, Eq a, Show a, FromObject b k v)
        => a -- ^ key
        -> [(a, Object k v)]
        -> m b
oLookup key pairs =
    case lookup key pairs of
        Nothing -> fail $ "Key not found: " ++ show key
        Just x -> fromObject x

-- instances

instance ToRaw Day where
    toRaw = toLazyByteString . show
instance ToObject Day key Raw where
    toObject = toObject . toRaw
instance FromRaw Day where
    fromRaw bs = do
        let s = fromLazyByteString bs
        if length s /= 10
            then fail ("Invalid day: " ++ s)
            else do
                let x = do
                    y' <- readMay $ take 4 s
                    m' <- readMay $ take 2 $ drop 5 s
                    d' <- readMay $ take 2 $ drop 8 s
                    return (y', m', d')
                case x of
                    Just (y, m, d) -> return $ fromGregorian y m d
                    Nothing -> fail $ "Invalid day: " ++ s
instance FromObject Day key Raw where
    fromObject o = fromObject o >>= fromRaw

instance ToRaw Bool where
    toRaw b = toRaw $ if b then "true" else "false"
instance ToObject Bool key Raw where
    toObject = toObject . toRaw
instance FromRaw Bool where
    fromRaw bs =
        case fromLazyByteString bs of
            "true" -> return True
            "false" -> return False
            x -> fail $ "Invalid bool value: " ++ x
instance FromObject Bool key Raw where
    fromObject o = fromObject o >>= fromRaw

instance ToRaw Int where
    toRaw = toRaw . show
instance ToObject Int key Raw where
    toObject = toObject . toRaw
instance FromRaw Int where
    fromRaw bs =
        case readMay $ fromLazyByteString bs of
            Nothing -> fail $ "Invalid integer: " ++ fromLazyByteString bs
            Just i -> return i
instance FromObject Int key Raw where
    fromObject o = fromObject o >>= fromRaw
