{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module GenericMappings where

import GHC.Generics
import Bridge.Intermediate
import Data.Text
import Data.Proxy

class TypescriptType a where
  toTypescriptType :: a -> Maybe TSType
  default toTypescriptType :: (Generic a, GenericTypescriptType (Rep a)) => a -> Maybe TSType
  toTypescriptType = gTSRep . from

class GenericTypescriptType f where
  gTSRep :: f a -> Maybe TSType

class GenericTSFields f where
  toTypescriptFields:: f a -> Maybe [TSField]


instance (Datatype d, Constructor c, GenericTSFields f) => GenericTypescriptType (M1 D d (M1 C c f)) where
  gTSRep d =
    if conIsRecord . unM1 $ d then
       TSInterface (pack $ datatypeName d) <$> toTypescriptFields (unM1 . unM1 $ d)
    else
      Nothing

instance (Selector s, GenericTypescriptType f) => GenericTSFields (S1 s f) where
  toTypescriptFields d =
    case selName d of
      "" -> Nothing
      name ->
        (gTSRep (unM1 d)) >>= (\t -> Just [TSField (FieldName $ pack name) t])

instance (GenericTSFields f, GenericTSFields g)
  => GenericTSFields (f :*: g) where
  toTypescriptFields _ =
    mappend
      <$> toTypescriptFields (undefined :: f p)
      <*> toTypescriptFields (undefined :: g p)

instance TypescriptType a => GenericTypescriptType (Rec0 a) where
  gTSRep _ = toTypescriptType (Proxy :: Proxy a)

instance (TypescriptType a) => TypescriptType (Proxy a) where
  toTypescriptType _ = toTypescriptType (undefined :: a)

instance TypescriptType a => TypescriptType (Maybe a) where
  toTypescriptType _ =
      TSOption <$> toTypescriptType (Proxy :: Proxy a)

instance TypescriptType Int where
  toTypescriptType _ = Just $ TSPrimitiveType TSNumber

instance TypescriptType Text where
  toTypescriptType _ = Just $ TSPrimitiveType TSString

instance TypescriptType String where
  toTypescriptType _ = Just $ TSPrimitiveType TSString

instance TypescriptType Bool where
  toTypescriptType _ = Just $ TSPrimitiveType TSBoolean

instance TypescriptType a => TypescriptType [a] where
  toTypescriptType _ = (TSCollectionType . TSArray) <$> toTypescriptType (Proxy :: Proxy a)
