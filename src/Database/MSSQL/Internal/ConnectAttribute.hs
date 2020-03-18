{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE KindSignatures             #-}

module Database.MSSQL.Internal.ConnectAttribute
       ( ConnectAttr
           ( SQL_ATTR_ACCESS_MODE_READ_ONLY
           , SQL_ATTR_ACCESS_MODE_READ_WRITE
           , SQL_ATTR_ASYNC_ENABLE_ON
           , SQL_ATTR_ASYNC_ENABLE_OFF
           , SQL_ATTR_CONNECTION_TIMEOUT
           , SQL_ATTR_CURRENT_CATALOG
           , SQL_ATTR_LOGIN_TIMEOUT
           , SQL_ATTR_METADATA_ID
           , SQL_ATTR_ODBC_CURSORS_CUR_USE_IF_NEEDED
           , SQL_ATTR_ODBC_CURSORS_CUR_USE_ODBC
           , SQL_ATTR_ODBC_CURSORS_CUR_USE_DRIVER
           , SQL_ATTR_PACKET_SIZE
           , SQL_ATTR_TRACE_ON
           , SQL_ATTR_TRACE_OFF
           , SQL_ATTR_TRACEFILE           
           , SQL_ATTR_TRANSLATE_LIB
           , SQL_ATTR_TRANSLATE_OPTION
           , SQL_ATTR_TXN_ISOLATION
           )
       , ConnectAttrAt (..)
       , connectAttrPtr
       ) where

import Foreign.ForeignPtr
import qualified Data.ByteString.Internal as BI
import Data.ByteString
import Foreign.C.Types
import Foreign.Storable
import qualified Language.C.Inline as C
import Database.MSSQL.Internal.Ctx

C.context $ mssqlCtx
  [ ("SQLUINTEGER", [t|CULong|])
  , ("SQLULEN", [t|CULong|])
  ]

C.include "<sqlext.h>"

-- TODO: getConnectAttr is a CInt
-- but SQLSetConnectAttr expects a CLong
-- attr = fromIntegral (getConnectAttr connAttr)
connectAttrPtr :: ConnectAttr a -> IO (CLong, ForeignPtr (), CLong)
connectAttrPtr connAttr =
  let attr = fromIntegral (getConnectAttr connAttr)
  in case attrValuePtr connAttr of
    AttrValue v -> mkVoidPtrFromLong attr v
    AttrPtr (String t) -> mkVoidPtrFromString attr t
    AttrPtr (Bytes b)  -> mkVoidPtrFromBytes attr b
  
    where mkVoidPtrFromLong attr v = do
            fptr <- mallocForeignPtr
            withForeignPtr fptr (\ptr -> poke ptr v) 
            pure (attr, castForeignPtr fptr, 0)

          -- TODO: Validate the following funs
          mkVoidPtrFromString attr t = do
            let (fptr, _, len) = BI.toForeignPtr t
            pure (attr, castForeignPtr fptr, fromIntegral len)
            
          mkVoidPtrFromBytes attr b = do
            let (fptr, _, len) = BI.toForeignPtr b
            pure (attr, castForeignPtr fptr, fromIntegral len)
            
data AttrValuePtr =
    AttrValue CULong
  | AttrPtr   AttrPtrType
  deriving (Show, Eq)

data AttrPtrType =
    String ByteString -- ^ CHAR8
  | Bytes ByteString
  -- | EvHandle SQLEVENT
  -- | WinHandle SQLWINDOW
  deriving (Show, Eq)

data ConnectAttrAt =
    ConnectBefore
  | ConnectAfter

data ConnectAttr (at :: ConnectAttrAt)
  = ConnectAttr
  { getConnectAttr :: CInt
  , attrValuePtr   :: AttrValuePtr
  } deriving (Show, Eq)

attrValue :: AttrValuePtr -> Maybe CULong
attrValue avp =
  case avp of
    AttrValue v -> Just v
    _           -> Nothing

attrPtr :: AttrValuePtr -> Maybe AttrPtrType
attrPtr avp =
  case avp of
    AttrPtr v -> Just v
    _         -> Nothing

attrPtrString :: AttrPtrType -> Maybe ByteString
attrPtrString (String t) = Just t
attrPtrString _          = Nothing

attrPtrBytes :: AttrPtrType -> Maybe ByteString
attrPtrBytes (Bytes t) = Just t
attrPtrBytes _         = Nothing

attrWithStringMaybePat :: CInt -> ConnectAttr a -> Maybe ByteString
attrWithStringMaybePat a attr =
  if a == getConnectAttr attr
  then attrPtrString =<< attrPtr (attrValuePtr attr)
  else Nothing

attrWithBytesMaybePat :: CInt -> ConnectAttr a -> Maybe ByteString
attrWithBytesMaybePat a attr =
  if a == getConnectAttr attr
  then attrPtrBytes =<< attrPtr (attrValuePtr attr)
  else Nothing

attrWithValueMaybePat :: CInt -> ConnectAttr a -> Maybe CULong
attrWithValueMaybePat a attr =
  if a == getConnectAttr attr
  then attrValue (attrValuePtr attr)
  else Nothing

attrWithValuePat :: CInt -> CULong -> ConnectAttr a -> Bool
attrWithValuePat a v attr = attrWithValue a v == attr

attrWithValue :: CInt -> CULong -> ConnectAttr a
attrWithValue a v = ConnectAttr a (AttrValue v)

attrWithString :: CInt -> ByteString -> ConnectAttr a
attrWithString a v = ConnectAttr a (AttrPtr (String v))

attrWithBytes :: CInt -> ByteString -> ConnectAttr a
attrWithBytes a v = ConnectAttr a (AttrPtr (Bytes v))

pattern SQL_ATTR_ACCESS_MODE_READ_ONLY :: ConnectAttr a
pattern SQL_ATTR_ACCESS_MODE_READ_ONLY <-
  (attrWithValuePat [C.pure| int {SQL_ATTR_ACCESS_MODE} |] [C.pure| SQLUINTEGER { SQL_MODE_READ_ONLY } |] -> True) where
  SQL_ATTR_ACCESS_MODE_READ_ONLY = attrWithValue [C.pure| int {SQL_ATTR_ACCESS_MODE} |] [C.pure| SQLUINTEGER { SQL_MODE_READ_ONLY } |]

pattern SQL_ATTR_ACCESS_MODE_READ_WRITE :: ConnectAttr a
pattern SQL_ATTR_ACCESS_MODE_READ_WRITE <-
  (attrWithValuePat [C.pure| int {SQL_ATTR_ACCESS_MODE} |] [C.pure| SQLUINTEGER { SQL_MODE_READ_WRITE } |] -> True) where
  SQL_ATTR_ACCESS_MODE_READ_WRITE = attrWithValue [C.pure| int {SQL_ATTR_ACCESS_MODE} |] [C.pure| SQLUINTEGER { SQL_MODE_READ_WRITE } |]

-- SQL_ATTR_ASYNC_DBC_EVENT
-- SQL_ATTR_DBC_FUNCTIONS_ENABLE
-- SQL_ATTR_ASYNC_DBC_PCALLBACK [Only driver manager can call]
-- SQL_ATTR_ASYNC_DBC_PCONTEXT  [Only driver manager can call]

pattern SQL_ATTR_ASYNC_ENABLE_ON :: ConnectAttr a
pattern SQL_ATTR_ASYNC_ENABLE_ON <-
  (attrWithValuePat [C.pure| int {SQL_ATTR_ASYNC_ENABLE} |] [C.pure| SQLULEN {SQL_ASYNC_ENABLE_ON} |] -> True) where
  SQL_ATTR_ASYNC_ENABLE_ON = attrWithValue [C.pure| int {SQL_ATTR_ASYNC_ENABLE} |] [C.pure| SQLUINTEGER {SQL_ASYNC_ENABLE_ON} |]

pattern SQL_ATTR_ASYNC_ENABLE_OFF :: ConnectAttr a
pattern SQL_ATTR_ASYNC_ENABLE_OFF <-
  (attrWithValuePat [C.pure| int {SQL_ATTR_ASYNC_ENABLE} |] [C.pure| SQLULEN {SQL_ASYNC_ENABLE_OFF} |] -> True) where
  SQL_ATTR_ASYNC_ENABLE_OFF = attrWithValue [C.pure| int {SQL_ATTR_ASYNC_ENABLE} |] [C.pure| SQLUINTEGER {SQL_ASYNC_ENABLE_OFF} |]

-- SQL_ATTR_AUTO_IPD cannot be set by SQLSetConnectAttr
-- should SQL_ATTR_AUTOCOMMIT be exposed?
-- SQL_ATTR_CONNECTION_DEAD read only

pattern SQL_ATTR_CONNECTION_TIMEOUT :: CULong -> ConnectAttr a
pattern SQL_ATTR_CONNECTION_TIMEOUT i <-
  (attrWithValueMaybePat [C.pure| int {SQL_ATTR_CONNECTION_TIMEOUT} |] -> Just i) where
  SQL_ATTR_CONNECTION_TIMEOUT i = attrWithValue [C.pure| int {SQL_ATTR_CONNECTION_TIMEOUT} |] i

pattern SQL_ATTR_CURRENT_CATALOG :: ByteString -> ConnectAttr a
pattern SQL_ATTR_CURRENT_CATALOG i <-
  (attrWithStringMaybePat [C.pure| int {SQL_ATTR_CURRENT_CATALOG} |] -> Just i) where
  SQL_ATTR_CURRENT_CATALOG i = attrWithString [C.pure| int {SQL_ATTR_CURRENT_CATALOG} |] i

-- SQL_DBC_INFO_TOKEN 
-- SQL_ATTR_ENLIST_IN_DTC

pattern SQL_ATTR_LOGIN_TIMEOUT :: CULong -> ConnectAttr 'ConnectBefore
pattern SQL_ATTR_LOGIN_TIMEOUT i <-
  (attrWithValueMaybePat [C.pure| int {SQL_ATTR_LOGIN_TIMEOUT} |] -> Just i) where
  SQL_ATTR_LOGIN_TIMEOUT i = attrWithValue [C.pure| int {SQL_ATTR_LOGIN_TIMEOUT} |] i

pattern SQL_ATTR_METADATA_ID :: CULong -> ConnectAttr a
pattern SQL_ATTR_METADATA_ID i <-
  (attrWithValueMaybePat [C.pure| int {SQL_ATTR_METADATA_ID} |] -> Just i) where
  SQL_ATTR_METADATA_ID i = attrWithValue [C.pure| int {SQL_ATTR_METADATA_ID} |] i

pattern SQL_ATTR_ODBC_CURSORS_CUR_USE_IF_NEEDED :: ConnectAttr 'ConnectBefore
pattern SQL_ATTR_ODBC_CURSORS_CUR_USE_IF_NEEDED <-
  (attrWithValuePat [C.pure| int {SQL_ATTR_ODBC_CURSORS} |] [C.pure| SQLULEN { SQL_CUR_USE_IF_NEEDED } |] -> True) where
  SQL_ATTR_ODBC_CURSORS_CUR_USE_IF_NEEDED = attrWithValue [C.pure| int {SQL_ATTR_ODBC_CURSORS} |] [C.pure| SQLULEN { SQL_CUR_USE_IF_NEEDED } |]

pattern SQL_ATTR_ODBC_CURSORS_CUR_USE_ODBC :: ConnectAttr 'ConnectBefore
pattern SQL_ATTR_ODBC_CURSORS_CUR_USE_ODBC <-
  (attrWithValuePat [C.pure| int {SQL_ATTR_ODBC_CURSORS} |] [C.pure| SQLULEN { SQL_CUR_USE_ODBC } |] -> True) where
  SQL_ATTR_ODBC_CURSORS_CUR_USE_ODBC = attrWithValue [C.pure| int {SQL_ATTR_ODBC_CURSORS} |] [C.pure| SQLULEN { SQL_CUR_USE_ODBC } |]

pattern SQL_ATTR_ODBC_CURSORS_CUR_USE_DRIVER :: ConnectAttr 'ConnectBefore
pattern SQL_ATTR_ODBC_CURSORS_CUR_USE_DRIVER <-
  (attrWithValuePat [C.pure| int {SQL_ATTR_ODBC_CURSORS} |] [C.pure| SQLULEN { SQL_CUR_USE_DRIVER } |] -> True) where
  SQL_ATTR_ODBC_CURSORS_CUR_USE_DRIVER = attrWithValue [C.pure| int {SQL_ATTR_ODBC_CURSORS} |] [C.pure| SQLULEN { SQL_CUR_USE_DRIVER } |]

pattern SQL_ATTR_PACKET_SIZE :: CULong -> ConnectAttr 'ConnectBefore
pattern SQL_ATTR_PACKET_SIZE i <-
  (attrWithValueMaybePat [C.pure| int {SQL_ATTR_PACKET_SIZE} |] -> Just i) where
  SQL_ATTR_PACKET_SIZE i = attrWithValue [C.pure| int {SQL_ATTR_PACKET_SIZE} |] i

-- SQL_ATTR_QUIET_MODE

pattern SQL_ATTR_TRACE_ON :: ConnectAttr a
pattern SQL_ATTR_TRACE_ON <-
  (attrWithValuePat [C.pure| int {SQL_ATTR_TRACE} |] [C.pure| SQLUINTEGER { SQL_OPT_TRACE_ON } |] -> True) where
  SQL_ATTR_TRACE_ON = attrWithValue [C.pure| int {SQL_ATTR_TRACE} |] [C.pure| SQLUINTEGER { SQL_OPT_TRACE_ON } |]

pattern SQL_ATTR_TRACE_OFF :: ConnectAttr a
pattern SQL_ATTR_TRACE_OFF <-
  (attrWithValuePat [C.pure| int {SQL_ATTR_TRACE} |] [C.pure| SQLUINTEGER { SQL_OPT_TRACE_OFF } |] -> True) where
  SQL_ATTR_TRACE_OFF = attrWithValue [C.pure| int {SQL_ATTR_TRACE} |] [C.pure| SQLUINTEGER { SQL_OPT_TRACE_OFF } |]

pattern SQL_ATTR_TRACEFILE :: ByteString -> ConnectAttr a
pattern SQL_ATTR_TRACEFILE i <-
  (attrWithStringMaybePat [C.pure| int {SQL_ATTR_TRACEFILE} |] -> Just i) where
  SQL_ATTR_TRACEFILE i = attrWithString [C.pure| int {SQL_ATTR_TRACEFILE} |] i

pattern SQL_ATTR_TRANSLATE_LIB :: ByteString -> ConnectAttr 'ConnectAfter
pattern SQL_ATTR_TRANSLATE_LIB i <-
  (attrWithStringMaybePat [C.pure| int {SQL_ATTR_TRANSLATE_LIB} |] -> Just i) where
  SQL_ATTR_TRANSLATE_LIB i = attrWithString [C.pure| int {SQL_ATTR_TRANSLATE_LIB} |] i

pattern SQL_ATTR_TRANSLATE_OPTION :: ByteString -> ConnectAttr 'ConnectAfter
pattern SQL_ATTR_TRANSLATE_OPTION i <-
  (attrWithBytesMaybePat [C.pure| int {SQL_ATTR_TRANSLATE_OPTION} |] -> Just i) where
  SQL_ATTR_TRANSLATE_OPTION i = attrWithBytes [C.pure| int {SQL_ATTR_TRANSLATE_OPTION} |] i

pattern SQL_ATTR_TXN_ISOLATION :: ByteString -> ConnectAttr a
pattern SQL_ATTR_TXN_ISOLATION i <-
  (attrWithBytesMaybePat [C.pure| int {SQL_ATTR_TXN_ISOLATION} |] -> Just i) where
  SQL_ATTR_TXN_ISOLATION i = attrWithBytes [C.pure| int {SQL_ATTR_TXN_ISOLATION} |] i

{-
pattern SQL_ATTR_ASYNC_DBC_FUNCTIONS_ENABLE :: ConnectAttr
#ifdef mingw32_HOST_OS
pattern SQL_ATTR_ASYNC_DBC_FUNCTIONS_ENABLE <- ((const True) -> False)
# else
pattern SQL_ATTR_ASYNC_DBC_FUNCTIONS_ENABLE v <- (withUnsafeAttrValue -> Just v) where
  SQL_ATTR_ASYNC_DBC_FUNCTIONS_ENABLE i = ConnectAttr [C.pure| int {SQL_ATTR_ASYNC_DBC_FUNCTIONS_ENABLE} |] (AttrValue i)
#endif  
  
pattern SQL_ATTR_AUTOCOMMIT :: ConnectAttr
pattern SQL_ATTR_AUTOCOMMIT <- ((ConnectAttr [C.pure| int {SQL_ATTR_AUTOCOMMIT} |] ==) -> True) where
  SQL_ATTR_AUTOCOMMIT = ConnectAttr [C.pure| int {SQL_ATTR_AUTOCOMMIT} |]

pattern SQL_ATTR_DISCONNECT_BEHAVIOR :: ConnectAttr
pattern SQL_ATTR_DISCONNECT_BEHAVIOR <- ((ConnectAttr [C.pure| int {SQL_ATTR_DISCONNECT_BEHAVIOR} |] ==) -> True) where
  SQL_ATTR_DISCONNECT_BEHAVIOR = ConnectAttr [C.pure| int {SQL_ATTR_DISCONNECT_BEHAVIOR} |]

pattern SQL_ATTR_ENLIST_IN_DTC :: ConnectAttr
pattern SQL_ATTR_ENLIST_IN_DTC <- ((ConnectAttr [C.pure| int {SQL_ATTR_ENLIST_IN_DTC} |] ==) -> True) where
  SQL_ATTR_ENLIST_IN_DTC = ConnectAttr [C.pure| int {SQL_ATTR_ENLIST_IN_DTC} |]

pattern SQL_ATTR_ENLIST_IN_XA :: ConnectAttr
pattern SQL_ATTR_ENLIST_IN_XA <- ((ConnectAttr [C.pure| int {SQL_ATTR_ENLIST_IN_XA} |] ==) -> True) where
  SQL_ATTR_ENLIST_IN_XA = ConnectAttr [C.pure| int {SQL_ATTR_ENLIST_IN_XA} |]

pattern SQL_ATTR_QUIET_MODE :: ConnectAttr
pattern SQL_ATTR_QUIET_MODE <- ((ConnectAttr [C.pure| int {SQL_ATTR_QUIET_MODE} |] ==) -> True) where
  SQL_ATTR_QUIET_MODE = ConnectAttr [C.pure| int {SQL_ATTR_QUIET_MODE} |]

pattern SQL_ATTR_AUTO_IPD :: ConnectAttr
pattern SQL_ATTR_AUTO_IPD <- ((ConnectAttr [C.pure| int {SQL_ATTR_AUTO_IPD} |] ==) -> True) where
  SQL_ATTR_AUTO_IPD = ConnectAttr [C.pure| int {SQL_ATTR_AUTO_IPD} |]

-}

{-
pattern SQL_ATTR_ASYNC_DBC_PCONTEXT :: ConnectAttr
pattern SQL_ATTR_ASYNC_DBC_PCONTEXT <- ((ConnectAttr [C.pure| int {SQL_ATTR_ASYNC_DBC_PCONTEXT} |] ==) -> True) where
  SQL_ATTR_ASYNC_DBC_PCONTEXT = ConnectAttr [C.pure| int {SQL_ATTR_ASYNC_DBC_PCONTEXT} |]

pattern SQL_ATTR_ASYNC_DBC_PCALLBACK :: ConnectAttr
pattern SQL_ATTR_ASYNC_DBC_PCALLBACK <- ((ConnectAttr [C.pure| int {SQL_ATTR_ASYNC_DBC_PCALLBACK} |] ==) -> True) where
  SQL_ATTR_ASYNC_DBC_PCALLBACK = ConnectAttr [C.pure| int {SQL_ATTR_ASYNC_DBC_PCALLBACK} |]
  
pattern SQL_ATTR_ASYNC_DBC_EVENT :: ConnectAttr
pattern SQL_ATTR_ASYNC_DBC_EVENT <- ((ConnectAttr [C.pure| int {SQL_ATTR_ASYNC_DBC_EVENT} |] ==) -> True) where
  SQL_ATTR_ASYNC_DBC_EVENT = ConnectAttr [C.pure| int {SQL_ATTR_ASYNC_DBC_EVENT} |]

-}

#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE
   SQL_ATTR_ACCESS_MODE_READ_ONLY
 , SQL_ATTR_ACCESS_MODE_READ_WRITE
 , SQL_ATTR_ASYNC_ENABLE_ON
 , SQL_ATTR_ASYNC_ENABLE_OFF
 , SQL_ATTR_CONNECTION_TIMEOUT
 , SQL_ATTR_CURRENT_CATALOG
 , SQL_ATTR_LOGIN_TIMEOUT
 , SQL_ATTR_METADATA_ID
 , SQL_ATTR_ODBC_CURSORS_CUR_USE_IF_NEEDED
 , SQL_ATTR_ODBC_CURSORS_CUR_USE_ODBC
 , SQL_ATTR_ODBC_CURSORS_CUR_USE_DRIVER
 , SQL_ATTR_PACKET_SIZE
 , SQL_ATTR_TRACE_ON
 , SQL_ATTR_TRACE_OFF
 , SQL_ATTR_TRACEFILE
 , SQL_ATTR_TRANSLATE_LIB
 , SQL_ATTR_TRANSLATE_OPTION
 , SQL_ATTR_TXN_ISOLATION
 :: ConnectAttr
 #-}
#endif
