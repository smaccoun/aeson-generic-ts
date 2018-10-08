module BridgeToTypescript where

import           Bridge.Intermediate
import           Data.Text           (Text)
import           Typescript.Types

bridgeTypeToTSType :: BType -> TSType
bridgeTypeToTSType btype =
  case btype of
    BPrimitiveType bprim ->
      TSPrimitiveType $ bprimToTSPrim bprim
    BCollectionType bcollection ->
      case bcollection of
        BArray btypeInArray ->
          TSCollectionType $ TSArray (bridgeTypeToTSType btypeInArray)
    BOption btypeInOption ->
      TSCustomType $ TSOption (bridgeTypeToTSType btypeInOption)
    BConstructed typeName c -> bConstructedToTS typeName c

bConstructedToTS :: Text -> BConstructor -> TSType
bConstructedToTS typeName bcon =
  case bcon of
    (SingleConstructorType fields) ->
      case fields of
        [x] ->
          case x of
            OfUnTagged btype -> bridgeTypeToTSType btype
            OfRecord bfield -> TSInterface typeName $ [bfieldToTSField bfield]
        _:_ -> TSInterface typeName $ fmap handleSingle fields
        _ -> TSAny
    (UnionConstructor cs) ->
      TSUnion typeName $ bConstructedToTS typeName <$> cs
  where
    handleSingle c =
      case c of
        OfRecord f -> bfieldToTSField f
        OfUnTagged _ -> TSField (FieldName "baloney") TSAny --TODO handle this

bprimToTSPrim :: BPrimitive -> TSPrimitive
bprimToTSPrim bprim =
  case bprim of
    BInt     -> TSNumber
    BString  -> TSString
    BBoolean -> TSBoolean

bfieldToTSField :: BField -> TSField
bfieldToTSField (BField (BFieldName fieldName ) btype) =
  TSField (Typescript.Types.FieldName fieldName) (bridgeTypeToTSType btype)
