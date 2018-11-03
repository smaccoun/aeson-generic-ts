module Internal.Intermediate.BridgeToTypescript where

import           Data.Text
import           Internal.Intermediate.Bridge.Lang
import           Internal.Intermediate.Typescript.Lang
import           Internal.Output.Foreign.Class
import           Internal.Output.Foreign.TSDefaults    ()

instance (IsForeignType (TSComposite f)) => FromBridge (TSIntermediate f) where
  toForeign btype =
    case btype of
      BPrimitiveType bprim ->
        Just $ TSPrimitiveType $ bprimToTSPrim bprim
      BCollectionType bcollection ->
        case bcollection of
          BArray btypeInArray ->
            TSCompositeType . TSCollection . TSArray <$> toForeign btypeInArray
      BOption btypeInOption ->
        TSCompositeType . TSOption <$> toForeign btypeInOption
      BConstructed typeName c -> bConstructedToTS toForeign typeName c

bprimToTSPrim :: BPrimitive -> TSPrimitive
bprimToTSPrim bprim = case bprim of
  BInt     -> TSNumber
  BString  -> TSString
  BBoolean -> TSBoolean


bConstructedToTS
  :: IsForeignType (TSComposite f)
  => (BType -> Maybe (TSIntermediate f))
  -> Text
  -> BConstructor
  -> Maybe (TSIntermediate f)
bConstructedToTS bridgeTypeToTSIntermediate typeName bcon = case bcon of
  (SingleConstructorType fields) -> case fields of
    [x] -> case x of
      OfUnTagged btype -> bridgeTypeToTSIntermediate btype
      OfRecord bfield ->
        TSCompositeType
          <$> TSDataType
          <$> TSData typeName
          <$> sequence [bfieldToTSField bfield]
    _ : _ ->
      TSCompositeType
        <$> TSDataType
        <$> TSData typeName
        <$> mapM handleSingle fields
    _ -> Nothing
  (UnionConstructor _) ->
    TSCompositeType
      <$> (Just $ TSUnionRef typeName [TSPrimitiveType TSString]) -- <$> (sequence $ bridgeTypeToTSIntermediate typeName <$> cs)
 where
  handleSingle
    :: IsForeignType (TSComposite f)
    => BSingleConstructorArg
    -> Maybe (TSField f)
  handleSingle c = case c of
    OfRecord   f -> bfieldToTSField f
    OfUnTagged _ -> Nothing

bfieldToTSField
  :: IsForeignType (TSComposite f)
  => BField
  -> Maybe (TSField f)
bfieldToTSField (BField (BFieldName fName) fType) =
  TSField (FieldName fName) <$> toForeign fType
