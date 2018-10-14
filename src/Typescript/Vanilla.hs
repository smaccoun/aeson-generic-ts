module Typescript.Vanilla where

import           Bridge.Intermediate
import           BridgeToTypescript
import           Data.Text
import           Typescript.Types

data Vanilla

instance FromBridge (TSType Vanilla) where
  toForeign btype =
    case btype of
      BPrimitiveType bprim ->
        Just $ TSPrimitiveType $ bprimToTSPrim bprim
      BCollectionType bcollection ->
        case bcollection of
          BArray btypeInArray ->
            TSCompositeType . TSCollection . TSArray <$> (toForeign btypeInArray)
      BOption btypeInOption ->
        TSCustomizableType . TSOption <$> (toForeign btypeInOption)
      BConstructed typeName c -> bConstructedToTS toForeign typeName c

bConstructedToTS :: (BType -> Maybe (TSType Vanilla)) -> Text -> BConstructor -> Maybe (TSType Vanilla)
bConstructedToTS bridgeTypeToTSType typeName bcon =
  case bcon of
    (SingleConstructorType fields) ->
      case fields of
        [x] ->
          case x of
            OfUnTagged btype -> bridgeTypeToTSType btype
            OfRecord bfield -> TSCompositeType <$> TSDataType <$> TSInterfaceRef <$> TSInterface typeName <$> (sequence [bfieldToTSField bfield])
--        _:_ -> TSInterface typeName <$> (mapM handleSingle fields)
        _ -> Nothing
    (UnionConstructor _) ->
      TSCustomizableType <$> (Just $ TSUnionRef typeName [TSPrimitiveType TSString]) -- <$> (sequence $ bridgeTypeToTSType typeName <$> cs)
  where
    bfieldToTSField :: (BField) -> Maybe (TSField Vanilla)
    bfieldToTSField (BField (BFieldName fName) fType) = TSField (FieldName fName) <$> toForeign fType
--    handleSingle :: BSingleConstructorArg -> Maybe (TSField Vanilla)
--    handleSingle c =
--      case c of
--        OfRecord f   -> bfieldToTSField f
--        OfUnTagged _ -> Nothing


instance IsForeignType (TSComposite Vanilla) where
  toForeignType (TSCollection tar) = toForeignType tar
  toForeignType (TSDataType _) = "INTERFACE"

instance IsForeignType (TSArray Vanilla) where
  toForeignType (TSArray tsType') = "Array<" <> toForeignType tsType' <> ">"

instance IsForeignType (TSCustom Vanilla) where
  toForeignType (TSOption tsType') = toForeignType tsType' <> " | null "
  toForeignType (TSUnionRef unionName tsTypes') =
    "type " <>  unionName <> " = " <> ns
    where
      ns =
         intercalate " | "
       $ fmap toForeignType tsTypes'


--data GenMany = forall a . TypescriptType a => GenMany a
--
--genTypescript :: [GenMany] -> Text
--genTypescript [] = ""
--genTypescript ((GenMany x):xs) =
--   (printTS x) <> "\n" <> genTypescript xs
--
--printTS :: (TypescriptType a) => a -> Text
--printTS tsType' =
--        fromMaybe ""
--    $   toTypescript
--    <$> toTypescriptType tsType'
