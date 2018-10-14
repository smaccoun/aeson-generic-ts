module BridgeToTypescript where

import           Bridge.Intermediate
import           Typescript.Types


bprimToTSPrim :: BPrimitive -> TSPrimitive
bprimToTSPrim bprim =
  case bprim of
    BInt     -> TSNumber
    BString  -> TSString
    BBoolean -> TSBoolean
