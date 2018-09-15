{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module GenericMappings where

import GHC.Generics
import Typescript.Types
import Data.Text

class TypescriptType a where
  toTypescriptType :: a -> Maybe TSType
  default toTypescriptType :: (Generic a, GRepToTSType (Rep a)) => a -> Maybe TSType
  toTypescriptType = gTSRep . from

class GRepToTSType f where
  gTSRep :: f a -> Maybe TSType

class GenericTSFields f where
  toTypescriptFields :: f a -> Maybe [TSField]

instance (Datatype d, Constructor c, GenericTSFields f) => GRepToTSType (M1 D d (M1 C c f)) where
  gTSRep d =
    if conIsRecord . unM1 $ d then
       TSInterface (pack $ datatypeName d) <$> toTypescriptFields (unM1 . unM1 $ d)
    else
      Nothing

instance (Selector s, GRepToTSType f) => GenericTSFields (S1 s f) where
  toTypescriptFields d =
    case selName d of
      "" -> Nothing
      name -> sequence $ [TSField (FieldName $ pack name) <$> (gTSRep (unM1 d))]

instance (GenericTSFields f, GenericTSFields g)
  => GenericTSFields (f :*: g) where
  toTypescriptFields _ =
    mappend
      <$> toTypescriptFields (undefined :: f p)
      <*> toTypescriptFields (undefined :: g p)


instance TypescriptType a => GRepToTSType (Rec0 a) where
  gTSRep _ = toTypescriptType (undefined :: a)

instance TypescriptType Int where
  toTypescriptType _ = Just $ TSPrimitiveType TSNumber

instance TypescriptType Text where
  toTypescriptType _ = Just $ TSPrimitiveType TSString

instance TypescriptType String where
  toTypescriptType _ = Just $ TSPrimitiveType TSString
