{-# LANGUAGE DeriveAnyClass #-}

module PrintForeign where

import           Bridge.Generics
import           Bridge.Intermediate
import           BridgeToTypescript()
import           Control.Monad.Catch
import           Data.Proxy
import           Data.Text           (Text)
import           Data.Typeable       (Typeable)
import           Typescript.Types
import           Typescript.Vanilla


data TranslateException = TranslateException Text
  deriving (Typeable)

instance Exception TranslateException

instance Show TranslateException where
  show (TranslateException t) = concat
    [ "Unable to parse as "
    , show t
    ]

asTS :: (BridgeType a, MonadThrow m) => Proxy a -> m (TSType Vanilla)
asTS bType =
  case toForeign (toBridgeType bType) of
    Just tsType -> return tsType
    Nothing     -> throwM $ TranslateException "Could not translate type"

printFromBridge :: (BridgeType a, MonadThrow m) => Proxy a -> m Text
printFromBridge t = (declaration . toForeignType) <$> asTS t
