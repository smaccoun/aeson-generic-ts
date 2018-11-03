module Internal.Output.PrintForeign where

import           Control.Monad.Catch
import           Data.Proxy
import           Data.Text                                (Text)
import           Data.Typeable                            (Typeable)
import           Internal.Intermediate.Bridge.Generics
import           Internal.Intermediate.Bridge.Lang
import           Internal.Intermediate.BridgeToTypescript ()
import           Internal.Intermediate.Typescript.Lang
import           Internal.Output.Foreign.Class

newtype TranslateException = TranslateException Text
  deriving (Typeable)

instance Exception TranslateException

instance Show TranslateException where
  show (TranslateException t) = "Unable to parse as " <> show t

{-TODO: Make this type safe so no exception needed -}
asTS
  :: (BridgeType a, MonadThrow m, FromBridge (TSIntermediate f))
  => Proxy f
  -> Proxy a
  -> m (TSIntermediate f)
asTS _ bType = case toForeign (toBridgeType bType) of
  Just tsType -> return tsType
  Nothing     -> throwM $ TranslateException "Could not translate type"

printFromBridge
  :: (BridgeType a, MonadThrow m, FromBridge (TSIntermediate f))
  => Proxy f
  -> Proxy a
  -> m Text
printFromBridge flavor tsType' = declaration . toForeignType <$> asTS flavor tsType'
