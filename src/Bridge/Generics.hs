{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Bridge.Generics where

import GHC.Generics
import Bridge.Intermediate
import Data.Text
import Data.Proxy

class BridgeType a where
  toBridgeType :: a -> Maybe BType
  default toBridgeType :: (Generic a, GenericBridgeType (Rep a)) => a -> Maybe BType
  toBridgeType = gBRep . from

class GenericBridgeType f where
  gBRep :: f a -> Maybe BType

class GenericBFields f where
  toBridgeFields:: f a -> Maybe [BField]

instance (Datatype d, Constructor c, GenericBFields f) => GenericBridgeType (M1 D d (M1 C c f)) where
  gBRep d =
    if conIsRecord . unM1 $ d then
       BRecordType <$> BRecord (pack $ datatypeName d) <$> toBridgeFields (unM1 . unM1 $ d)
    else
      Nothing

instance (Selector s, GenericBridgeType f) => GenericBFields (S1 s f) where
  toBridgeFields d =
    case selName d of
      "" -> Nothing
      name ->
        (gBRep (unM1 d)) >>= (\t -> Just [BField (BFieldName $ pack name) t])

instance (GenericBFields f, GenericBFields g)
  => GenericBFields (f :*: g) where
  toBridgeFields _ =
    mappend
      <$> toBridgeFields (undefined :: f p)
      <*> toBridgeFields (undefined :: g p)

instance BridgeType a => GenericBridgeType (Rec0 a) where
  gBRep _ = toBridgeType (Proxy :: Proxy a)

instance (BridgeType a) => BridgeType (Proxy a) where
  toBridgeType _ = toBridgeType (undefined :: a)

instance BridgeType a => BridgeType (Maybe a) where
  toBridgeType _ =
      BOption <$> toBridgeType (Proxy :: Proxy a)

instance BridgeType Int where
  toBridgeType _ = Just $ BPrimitiveType BInt

instance BridgeType Text where
  toBridgeType _ = Just $ BPrimitiveType BString

instance BridgeType String where
  toBridgeType _ = Just $ BPrimitiveType BString

instance BridgeType Bool where
  toBridgeType _ = Just $ BPrimitiveType BBoolean

instance BridgeType a => BridgeType [a] where
  toBridgeType _ = (BCollectionType . BArray) <$> toBridgeType (Proxy :: Proxy a)
