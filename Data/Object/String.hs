{-# LANGUAGE TypeSynonymInstances #-}
---------------------------------------------------------
--
-- Module        : Data.Object.String
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
--
-- Objects with simple string keys and values
---------------------------------------------------------
module Data.Object.String
    ( StringObject
    , ToStringObject (..)
    , FromStringObject (..)
    ) where

import Data.Object.Raw
import qualified Data.ByteString.Lazy.Char8 as B

type StringObject = Object String String

class ToStringObject tso where
    toStringObject :: tso -> StringObject
instance ToStringObject StringObject where
    toStringObject = id
instance ToStringObject RawObject where
    toStringObject = mapKeysValues B.unpack B.unpack

class FromStringObject fso where
    fromStringObject :: StringObject -> fso
instance FromStringObject StringObject where
    fromStringObject = id
instance FromStringObject RawObject where
    fromStringObject = mapKeysValues B.pack B.pack
