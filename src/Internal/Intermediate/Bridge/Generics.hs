{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Internal.Intermediate.Bridge.Generics where

import GHC.Generics
import Internal.Intermediate.Bridge.Lang
import Data.Text
import Data.Proxy

class BridgeType a where
  toBridgeType :: a -> BType
  default toBridgeType :: (Generic a, GenericBridgeType (Rep a)) => a -> BType
  toBridgeType = gBRep . from

class GenericBridgeType f where
  gBRep :: f a -> BType

instance (Datatype d, GenericBridgeConstructor f) =>
         GenericBridgeType (D1 d f) where
  gBRep datatype =
    BConstructed typeName constructor
    where
      typeName = pack (datatypeName datatype)
      constructor = toBridgeConstructor (unM1 datatype)

class GenericBridgeConstructor f where
  toBridgeConstructor :: f a -> BConstructor

class GenericBSingleConstructorArg f where
  toBridgeSingleConstructorArg :: f a -> BSingleConstructorArg

class GenericBSingleConstructorArgs f where
  toBridgeSingleConstructorArgs :: f a -> [BSingleConstructorArg]

class GenericBField f where
  toBridgeField :: f a -> BField

instance (Constructor c, GenericBSingleConstructorArgs f) => GenericBridgeConstructor (M1 C c f) where
  toBridgeConstructor d =
      SingleConstructorType $ toBridgeSingleConstructorArgs (unM1 d)

instance (Selector s, GenericBridgeType f) => GenericBSingleConstructorArgs (S1 s f) where
  toBridgeSingleConstructorArgs d =
    case selName d of
      "" -> [OfUnTagged (gBRep (unM1 d))] --TODO: Figure this out
      name ->
        [OfRecord $ BField (BFieldName $ pack name) (gBRep (unM1 d))]

instance (GenericBSingleConstructorArgs f, GenericBSingleConstructorArgs g)
  => GenericBSingleConstructorArgs (f :*: g) where
  toBridgeSingleConstructorArgs _ =
    mappend t1 t2
    where
      t1 = toBridgeSingleConstructorArgs (undefined :: f p)
      t2 = toBridgeSingleConstructorArgs (undefined :: g p)

instance (GenericBridgeConstructor f, GenericBridgeConstructor g) => GenericBridgeConstructor (f :+: g) where
  toBridgeConstructor _ =
    UnionConstructor
      [toBridgeConstructor (undefined :: f p)
      ,toBridgeConstructor (undefined :: g p)
      ]

instance BridgeType a => GenericBridgeType (Rec0 a) where
  gBRep _ = toBridgeType (Proxy :: Proxy a)

instance (BridgeType a) => BridgeType (Proxy a) where
  toBridgeType _ = toBridgeType (undefined :: a)

instance BridgeType a => BridgeType (Maybe a) where
  toBridgeType _ =
      BOption $ toBridgeType (Proxy :: Proxy a)

instance BridgeType Int where
  toBridgeType _ = BPrimitiveType BInt

instance BridgeType Text where
  toBridgeType _ = BPrimitiveType BString

instance BridgeType String where
  toBridgeType _ = BPrimitiveType BString

instance BridgeType Bool where
  toBridgeType _ = BPrimitiveType BBoolean

instance BridgeType a => BridgeType [a] where
  toBridgeType _ = (BCollectionType . BArray) $ toBridgeType (Proxy :: Proxy a)
