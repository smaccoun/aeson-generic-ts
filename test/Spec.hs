{-# LANGUAGE DeriveAnyClass #-}

module Spec where

import           Bridge.Generics
import           Bridge.Intermediate
import           Control.Monad.Catch
import           Data.Proxy
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Typeable       (Typeable)
import           GHC.Generics
import           Test.Hspec
import           Typescript.Types
import           Typescript.Vanilla

data ComplexRecord =
  ComplexRecord
    {anIntField :: Int
    ,aTextField :: Text
--    ,aUnion     :: SampleUnion
--    ,aMaybeType :: Maybe Text
    } deriving (Generic, BridgeType)

data SimpleUnTagged = F Int deriving (Generic, BridgeType)

data SampleUnion = FirstCon Int | SecondCon Text deriving (Generic, BridgeType)

aesonGenericTSSpec :: Spec
aesonGenericTSSpec = do
  describe "translates_to_all_primitives" $ do
    it "works for number" $ do
      t <- printFromBridge (Proxy :: Proxy Int)
      t `shouldBe` "number"

    it "works for number" $ do
      t <- printFromBridge (Proxy :: Proxy [Int])
      t `shouldBe` "Array<number>"

--    it "Should match the given TStype from Bridge" $
--      asTS (Proxy :: Proxy ComplexRecord) `shouldBe` (TSInterface
--                   "ComplexRecord"
--                   [ TSField (FieldName "anIntField") (TSPrimitiveType TSNumber)
--                   , TSField (FieldName "aTextField") (TSPrimitiveType TSString)
--                   , TSField (FieldName "aUnion") (TSUnion "SampleUnion" [TSPrimitiveType TSNumber,TSPrimitiveType TSString])
--                   , TSField (FieldName "aMaybeType") (TSCustomType (TSOption (TSPrimitiveType TSString)))
--                   ]
--                 )

    it "Should output the correct complex record" $ do
      ts <- printFromBridge (Proxy :: Proxy ComplexRecord)
      ts `shouldBe` knownSolution
      where
        knownSolution =
          T.intercalate ""
            ["interface ComplexRecord { \n"
            ,"anIntField : number\n"
            ,"aTextField : string}"
--            ,"   aUnion : SampleUnion \n"
--            ,"   aMaybeType : string | null}"
            ]

--    it "Should output the correct vanilla union" $ do
--       printFromBridge (Proxy :: Proxy SampleUnion) `shouldBe` "type SampleUnion = number | string"

data TranslateException = TranslateException Text
  deriving (Typeable)

instance Exception TranslateException

instance Show TranslateException where
  show (TranslateException t) = concat
    [ "Unable to parse as "
--    , show typ
--    , ": "
    , show t
    ]

asTS :: (BridgeType a, MonadThrow m) => Proxy a -> m (TSType Vanilla)
asTS bType =
  case toForeign (toBridgeType bType) of
    Just tsType -> return tsType
    Nothing     -> throwM $ TranslateException "Could not translate type"

printFromBridge :: (BridgeType a, MonadThrow m) => Proxy a -> m Text
printFromBridge t = toForeignType <$> asTS t
