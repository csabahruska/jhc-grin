{- Generated by DrIFT (Automatic class derivations for Haskell) -}
{-# LINE 1 "src/C/FFI.hs" #-}
module C.FFI(
    CallConv(..),
    Safety(..),
    FfiType(..),
    FfiExport(..),
    FfiSpec(..),
    Requires(..)
    ) where

import C.Prims
import Data.Binary
import Data.Typeable

type CName = String

data FfiType = Import CName Requires
             | ImportAddr CName Requires
             | Wrapper
             | Dynamic
             deriving(Eq,Ord,Show)

data FfiSpec = FfiSpec FfiType Safety CallConv
             deriving(Eq,Ord,Show)

data FfiExport = FfiExport {
    ffiExportCName    :: CName,
    ffiExportSafety   :: Safety,
    ffiExportCallConv :: CallConv,
    ffiExportArgTypes :: [ExtType],
    ffiExportRetType  :: ExtType
    }
 deriving(Eq,Ord,Show,Typeable)
     {-! derive: Binary !-}
{-* Generated by DrIFT : Look, but Don't Touch. *-}
instance Data.Binary.Binary FfiExport where
    put (FfiExport aa ab ac ad ae) = do
	    Data.Binary.put aa
	    Data.Binary.put ab
	    Data.Binary.put ac
	    Data.Binary.put ad
	    Data.Binary.put ae
    get = do
    aa <- get
    ab <- get
    ac <- get
    ad <- get
    ae <- get
    return (FfiExport aa ab ac ad ae)

--  Imported from other files :-