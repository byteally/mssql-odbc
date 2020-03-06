{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.MSSQL.Internal.Ctx where

import qualified Data.Map as Map
import qualified Language.C.Inline as C
import qualified Language.C.Types as C
import Language.C.Inline.Context (ctxTypesTable)
import Language.Haskell.TH
#if __GLASGOW_HASKELL__ < 802
import Data.Monoid
#endif


mssqlCtx :: [(C.CIdentifier, TypeQ)] -> C.Context
mssqlCtx types = C.baseCtx <> C.bsCtx <> C.funCtx <> C.fptrCtx <>  C.vecCtx <> mssqlCtx'
  where
    mssqlCtx' = mempty
      { ctxTypesTable = mssqlTypesTable types
      }

mssqlTypesTable :: [(C.CIdentifier, TypeQ)] -> Map.Map C.TypeSpecifier TypeQ
mssqlTypesTable types = Map.fromList (
  [ (C.TypeName "SQLRETURN", [t| C.CInt |])
  , (C.TypeName "ConnectInfo", [t| C.CInt |])
  ] ++ (fmap (\(f,s) -> (C.TypeName f, s)) types))
