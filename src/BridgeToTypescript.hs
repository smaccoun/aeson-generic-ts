module BridgeToTypescript where

import           Bridge.Intermediate
import           Data.Text           (Text)
import           Typescript.Types

bridgeTypeToTSType :: BType -> Maybe TSType
bridgeTypeToTSType btype =
  case btype of
    BPrimitiveType bprim ->
      Just $ TSPrimitiveType $ bprimToTSPrim bprim
    BCollectionType bcollection ->
      case bcollection of
        BArray btypeInArray ->
          TSCollectionType . TSArray <$> (bridgeTypeToTSType btypeInArray)
    BOption btypeInOption ->
      TSCustomType . TSOption <$> (bridgeTypeToTSType btypeInOption)
    BConstructed typeName c -> bConstructedToTS typeName c

bConstructedToTS :: Text -> BConstructor -> Maybe TSType
bConstructedToTS typeName bcon =
  case bcon of
    (SingleConstructorType fields) ->
      case fields of
        [x] ->
          case x of
            OfUnTagged btype -> bridgeTypeToTSType btype
            OfRecord bfield -> TSInterface typeName <$> (sequence [bfieldToTSField bfield])
        _:_ -> TSInterface typeName <$> (mapM handleSingle fields)
        _ -> Nothing
    (UnionConstructor cs) ->
      TSUnion typeName <$> (sequence $ bConstructedToTS typeName <$> cs)
  where
    handleSingle :: BSingleConstructorArg -> Maybe TSField
    handleSingle c =
      case c of
        OfRecord f -> bfieldToTSField f
        OfUnTagged _ -> Nothing

bprimToTSPrim :: BPrimitive -> TSPrimitive
bprimToTSPrim bprim =
  case bprim of
    BInt     -> TSNumber
    BString  -> TSString
    BBoolean -> TSBoolean

bfieldToTSField :: BField -> Maybe TSField
bfieldToTSField (BField (BFieldName fieldName ) btype) =
  TSField (Typescript.Types.FieldName fieldName) <$> (bridgeTypeToTSType btype)
