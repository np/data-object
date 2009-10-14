{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
---------------------------------------------------------
--
-- Module        : Data.Object
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
--
-- These objects show up in different places, eg JSON, Yaml.
-- By providing a representation in a separate repository,
-- other libraries can share a single representation of
-- these structures.
--
---------------------------------------------------------
module Data.Object
    ( Object (..)
    , mapKeys
    , mapValues
    , mapKeysValues
    , mapKeysValuesM
    , MonadFail
    , ToObject(..)
    , FromObject(..)
    , getScalar
    , getSequence
    , getMapping
    ) where

import Control.Arrow
import Control.Applicative
import Control.Monad hiding (mapM)

import Prelude hiding (mapM, sequence)

import Data.Foldable
import Data.Traversable
import Data.Monoid

class (Functor m, Applicative m, Monad m) => MonadFail m where

instance MonadFail IO where
instance MonadFail Maybe where
instance MonadFail [] where

data Object key val =
    Mapping [(key, Object key val)]
    | Sequence [Object key val]
    | Scalar val
    deriving (Show, Eq)

mapKeys :: (key1 -> key2) -> Object key1 val -> Object key2 val
mapKeys = flip mapKeysValues id

-- | This is equivalent to 'fmap'.
mapValues :: (val1 -> val2) -> Object key val1 -> Object key val2
mapValues = mapKeysValues id

mapKeysValues :: (key1 -> key2)
              -> (val1 -> val2)
              -> Object key1 val1
              -> Object key2 val2
mapKeysValues _ fv (Scalar v) = Scalar $ fv v
mapKeysValues fk fv (Sequence os)= Sequence $ map (mapKeysValues fk fv) os
mapKeysValues fk fv (Mapping pairs) =
    Mapping $ map (fk *** mapKeysValues fk fv) pairs

mapKeysValuesM :: MonadFail m
               => (key1 -> m key2)
               -> (val1 -> m val2)
               -> Object key1 val1
               -> m (Object key2 val2)
mapKeysValuesM _ fv (Scalar v) = Scalar <$> fv v
mapKeysValuesM fk fv (Sequence os) =
    Sequence <$> mapM (mapKeysValuesM fk fv) os
mapKeysValuesM fk fv (Mapping pairs) = Mapping <$>
    mapM (uncurry (liftM2 (,)) . (fk *** mapKeysValuesM fk fv)) pairs

class ToObject a k v where
    toObject :: a -> Object k v
class FromObject a k v where
    fromObject :: MonadFail m => Object k v -> m a

instance ToObject a k v => ToObject [a] k v where
    toObject = Sequence . map toObject
instance FromObject a k v => FromObject [a] k v where
    fromObject = mapM fromObject <=< getSequence

-- use Data.Map or an Assoc newtype
instance ToObject a k v => ToObject [(k, a)] k v where
    toObject = Mapping . map (second toObject)
instance FromObject a k v => FromObject [(k, a)] k v where
    fromObject =
        mapM (runKleisli (second (Kleisli fromObject)))
          <=< getMapping

instance ToObject (Object key val) key val where
    toObject = id
instance FromObject (Object key val) key val where
    fromObject = return

instance Functor (Object key) where
    fmap = mapValues

instance Foldable (Object key) where
    foldMap f (Scalar v) = f v
    foldMap f (Sequence vs) = mconcat $ map (foldMap f) vs
    foldMap f (Mapping pairs) = mconcat $ map (foldMap f . snd) pairs

instance Traversable (Object key) where
    traverse f (Scalar v) = Scalar <$> f v
    traverse f (Sequence vs) = Sequence <$> traverse (traverse f) vs
    traverse f (Mapping pairs) =
      Mapping <$> traverse (traverse' (traverse f)) pairs

-- It would be nice if there were an "instance Traversable ((,) a)", but I
-- won't make an orphan instance simply for convenience. Instead:
traverse' :: Applicative f => (a -> f b) -> (x, a) -> f (x, b)
traverse' f (x, a) = (,) x <$> f a

joinObj :: Object key (Object key scalar) -> Object key scalar
joinObj (Scalar x)    = x
joinObj (Sequence xs) = Sequence (map joinObj xs)
joinObj (Mapping  xs) = Mapping  (map (second joinObj) xs)

instance Monad (Object key) where
    return = Scalar
    x >>= f = joinObj . fmap f $ x

instance Applicative (Object key) where
    pure  = Scalar
    (<*>) = ap

getScalar :: MonadFail m => Object k v -> m v
getScalar (Scalar s) = return s
getScalar _ = fail "Attempt to extract a scalar from non-scalar"

getSequence :: MonadFail m => Object k v -> m [Object k v]
getSequence (Sequence s) = return s
getSequence _ = fail "Attempt to extract a sequence from non-sequence"

getMapping :: MonadFail m => Object k v -> m [(k, Object k v)]
getMapping (Mapping m) = return m
getMapping _ = fail "Attempt to extract a mapping from non-mapping"
