module Internal.Intermediate.BridgeToTypescript where

import           Data.Text
import           Internal.Intermediate.Bridge.Lang
import           Internal.Intermediate.Typescript.Lang
import           Internal.Output.Foreign.Class
import           Internal.Output.Foreign.TSDefaults    ()

instance (IsForeignType (TSCustom f), IsForeignType (TSComposite f)) => FromBridge (TSType f) where
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

bprimToTSPrim :: BPrimitive -> TSPrimitive
bprimToTSPrim bprim = case bprim of
  BInt     -> TSNumber
  BString  -> TSString
  BBoolean -> TSBoolean


bConstructedToTS
  :: (IsForeignType (TSCustom f), IsForeignType (TSComposite f))
  => (BType -> Maybe (TSType f))
  -> Text
  -> BConstructor
  -> Maybe (TSType f)
bConstructedToTS bridgeTypeToTSType typeName bcon = case bcon of
  (SingleConstructorType fields) -> case fields of
    [x] -> case x of
      OfUnTagged btype -> bridgeTypeToTSType btype
      OfRecord bfield ->
        TSCompositeType
          <$> TSDataType
          <$> TSInterfaceRef
          <$> TSInterface typeName
          <$> (sequence [bfieldToTSField bfield])
    _ : _ ->
      TSCompositeType
        <$> TSDataType
        <$> TSInterfaceRef
        <$> TSInterface typeName
        <$> (mapM handleSingle fields)
    _ -> Nothing
  (UnionConstructor _) ->
    TSCustomizableType
      <$> (Just $ TSUnionRef typeName [TSPrimitiveType TSString]) -- <$> (sequence $ bridgeTypeToTSType typeName <$> cs)
 where
  handleSingle
    :: (IsForeignType (TSCustom f), IsForeignType (TSComposite f))
    => BSingleConstructorArg
    -> Maybe (TSField f)
  handleSingle c = case c of
    OfRecord   f -> bfieldToTSField f
    OfUnTagged _ -> Nothing

bfieldToTSField
  :: (IsForeignType (TSCustom f), IsForeignType (TSComposite f))
  => (BField)
  -> Maybe (TSField f)
bfieldToTSField (BField (BFieldName fName) fType) =
  TSField (FieldName fName) <$> toForeign fType
