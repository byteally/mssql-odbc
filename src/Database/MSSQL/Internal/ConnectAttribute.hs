{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
       , AttrName
           ( SQL_ATTR_NAME_ACCESS_MODE
           , SQL_ATTR_NAME_ASYNC_DBC_FUNCTIONS_ENABLE
           , SQL_ATTR_NAME_ASYNC_ENABLE
           , SQL_ATTR_NAME_AUTO_IPD
           , SQL_ATTR_NAME_AUTOCOMMIT
           , SQL_ATTR_NAME_CONNECTION_DEAD
           , SQL_ATTR_NAME_CONNECTION_TIMEOUT
           , SQL_ATTR_NAME_CURRENT_CATALOG
           , SQL_ATTR_NAME_ENLIST_IN_DTC
           , SQL_ATTR_NAME_LOGIN_TIMEOUT
           , SQL_ATTR_NAME_METADATA_ID
           , SQL_ATTR_NAME_ODBC_CURSORS
           , SQL_ATTR_NAME_PACKET_SIZE
           , SQL_ATTR_NAME_QUIET_MODE
           , SQL_ATTR_NAME_TRACE
           , SQL_ATTR_NAME_TRACEFILE
           , SQL_ATTR_NAME_TRANSLATE_LIB
           , SQL_ATTR_NAME_TRANSLATE_OPTION
           , SQL_ATTR_NAME_TXN_ISOLATION
           )           
       , ConnectAttrAt (..)
       , setConnectAttrPtr
       , getConnectAttrPtr
       , isFixedSizeAttrValue
       , getAttrName
       ) where

import Foreign.ForeignPtr
import qualified Data.ByteString.Internal as BI
import Data.ByteString (ByteString, packCStringLen)
import Foreign.C.Types
import Foreign.Storable
import qualified Language.C.Inline as C
import Database.MSSQL.Internal.Ctx

C.context $ mssqlCtx
  [ ("SQLUINTEGER", [t|CULong|])
  , ("SQLULEN", [t|CULong|])
  ]

#ifdef mingw32_HOST_OS
C.include "<windows.h>"
#endif
C.include "<stdio.h>"
C.include "<stdlib.h>"
C.include "<sqlext.h>"
C.include "<sqltypes.h>"
C.include "<sqlucode.h>"
C.include "<ss.h>"

setConnectAttrPtr :: ConnectAttr a -> IO (CLong, ForeignPtr (), CLong)
setConnectAttrPtr connAttr =
  let attr = fromIntegral (getConnectAttrName connAttr)
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

getConnectAttrPtr :: AttrName -> ForeignPtr () -> ForeignPtr CLong -> IO (Maybe (ConnectAttr a))
getConnectAttrPtr attr vptr lenPtr =
  case attr of
    SQL_ATTR_NAME_ACCESS_MODE -> Just <$> attrWithValuePtr SQL_ATTR_NAME_ACCESS_MODE vptr
    SQL_ATTR_NAME_ASYNC_ENABLE -> Just <$> attrWithValuePtr SQL_ATTR_NAME_ASYNC_ENABLE vptr
    SQL_ATTR_NAME_CONNECTION_TIMEOUT -> Just <$> attrWithValuePtr SQL_ATTR_NAME_CONNECTION_TIMEOUT vptr
    SQL_ATTR_NAME_CURRENT_CATALOG -> Just <$> attrWithStringPtr SQL_ATTR_NAME_CURRENT_CATALOG vptr lenPtr
    SQL_ATTR_NAME_LOGIN_TIMEOUT -> Just <$> attrWithValuePtr SQL_ATTR_NAME_LOGIN_TIMEOUT vptr
    SQL_ATTR_NAME_METADATA_ID -> Just <$> attrWithValuePtr SQL_ATTR_NAME_METADATA_ID vptr
    SQL_ATTR_NAME_ODBC_CURSORS -> Just <$> attrWithValuePtr SQL_ATTR_NAME_ODBC_CURSORS vptr
    SQL_ATTR_NAME_PACKET_SIZE -> Just <$> attrWithValuePtr SQL_ATTR_NAME_PACKET_SIZE vptr
    SQL_ATTR_NAME_TRACE -> Just <$> attrWithValuePtr SQL_ATTR_NAME_TRACE vptr
    SQL_ATTR_NAME_TRACEFILE -> Just <$> attrWithStringPtr SQL_ATTR_NAME_TRACEFILE vptr lenPtr
    SQL_ATTR_NAME_TRANSLATE_OPTION -> Just <$> attrWithBytesPtr SQL_ATTR_NAME_TRANSLATE_OPTION vptr lenPtr
    SQL_ATTR_NAME_TXN_ISOLATION -> Just <$> attrWithBytesPtr SQL_ATTR_NAME_TXN_ISOLATION vptr lenPtr
    _ -> pure Nothing

            
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
  { getConnectAttrName :: AttrName
  , attrValuePtr :: AttrValuePtr
  } deriving (Show, Eq)

newtype AttrName = AttrName { getAttrName :: CLong }
             deriving (Show, Eq, Storable, Integral, Real, Enum, Num, Ord)

attrNameEq :: CLong -> AttrName -> Bool
attrNameEq a b = AttrName a == b

pattern SQL_ATTR_NAME_ACCESS_MODE :: AttrName
pattern SQL_ATTR_NAME_ACCESS_MODE <-
  (attrNameEq [C.pure| long {SQL_ATTR_ACCESS_MODE} |] -> True) where
  SQL_ATTR_NAME_ACCESS_MODE = AttrName [C.pure| long {SQL_ATTR_ACCESS_MODE} |] 

{-
pattern SQL_ATTR_NAME_ASYNC_DBC_EVENT :: AttrName
pattern SQL_ATTR_NAME_ASYNC_DBC_EVENT <-
  (attrNameEq [C.pure| long {SQL_ATTR_ASYNC_DBC_EVENT} |] -> True) where
  SQL_ATTR_NAME_ASYNC_DBC_EVENT = AttrName [C.pure| long {SQL_ATTR_ASYNC_DBC_EVENT} |] 
-}

pattern SQL_ATTR_NAME_ASYNC_DBC_FUNCTIONS_ENABLE :: AttrName
pattern SQL_ATTR_NAME_ASYNC_DBC_FUNCTIONS_ENABLE <-
  (attrNameEq [C.pure| long {SQL_ATTR_ASYNC_DBC_FUNCTIONS_ENABLE} |] -> True) where
  SQL_ATTR_NAME_ASYNC_DBC_FUNCTIONS_ENABLE = AttrName [C.pure| long {SQL_ATTR_ASYNC_DBC_FUNCTIONS_ENABLE} |] 

{-
pattern SQL_ATTR_NAME_ASYNC_DBC_PCALLBACK :: AttrName
pattern SQL_ATTR_NAME_ASYNC_DBC_PCALLBACK <-
  (attrNameEq [C.pure| long {SQL_ATTR_ASYNC_DBC_PCALLBACK} |] -> True) where
  SQL_ATTR_NAME_ASYNC_DBC_PCALLBACK = AttrName [C.pure| long {SQL_ATTR_ASYNC_DBC_PCALLBACK} |] 

pattern SQL_ATTR_NAME_ASYNC_DBC_PCONTEXT :: AttrName
pattern SQL_ATTR_NAME_ASYNC_DBC_PCONTEXT <-
  (attrNameEq [C.pure| long {SQL_ATTR_ASYNC_DBC_PCONTEXT} |] -> True) where
  SQL_ATTR_NAME_ASYNC_DBC_PCONTEXT = AttrName [C.pure| long {SQL_ATTR_ASYNC_DBC_PCONTEXT} |] 
-}

pattern SQL_ATTR_NAME_ASYNC_ENABLE :: AttrName
pattern SQL_ATTR_NAME_ASYNC_ENABLE <-
  (attrNameEq [C.pure| long {SQL_ATTR_ASYNC_ENABLE} |] -> True) where
  SQL_ATTR_NAME_ASYNC_ENABLE = AttrName [C.pure| long {SQL_ATTR_ASYNC_ENABLE} |] 

pattern SQL_ATTR_NAME_AUTO_IPD :: AttrName
pattern SQL_ATTR_NAME_AUTO_IPD <-
  (attrNameEq [C.pure| long {SQL_ATTR_AUTO_IPD} |] -> True) where
  SQL_ATTR_NAME_AUTO_IPD = AttrName [C.pure| long {SQL_ATTR_AUTO_IPD} |] 

pattern SQL_ATTR_NAME_AUTOCOMMIT :: AttrName
pattern SQL_ATTR_NAME_AUTOCOMMIT <-
  (attrNameEq [C.pure| long {SQL_ATTR_AUTOCOMMIT} |] -> True) where
  SQL_ATTR_NAME_AUTOCOMMIT = AttrName [C.pure| long {SQL_ATTR_AUTOCOMMIT} |] 

pattern SQL_ATTR_NAME_CONNECTION_DEAD :: AttrName
pattern SQL_ATTR_NAME_CONNECTION_DEAD <-
  (attrNameEq [C.pure| long {SQL_ATTR_CONNECTION_DEAD} |] -> True) where
  SQL_ATTR_NAME_CONNECTION_DEAD = AttrName [C.pure| long {SQL_ATTR_CONNECTION_DEAD} |] 

pattern SQL_ATTR_NAME_CONNECTION_TIMEOUT :: AttrName
pattern SQL_ATTR_NAME_CONNECTION_TIMEOUT <-
  (attrNameEq [C.pure| long {SQL_ATTR_CONNECTION_TIMEOUT} |] -> True) where
  SQL_ATTR_NAME_CONNECTION_TIMEOUT = AttrName [C.pure| long {SQL_ATTR_CONNECTION_TIMEOUT} |] 

pattern SQL_ATTR_NAME_CURRENT_CATALOG :: AttrName
pattern SQL_ATTR_NAME_CURRENT_CATALOG <-
  (attrNameEq [C.pure| long {SQL_ATTR_CURRENT_CATALOG} |] -> True) where
  SQL_ATTR_NAME_CURRENT_CATALOG = AttrName [C.pure| long {SQL_ATTR_CURRENT_CATALOG} |] 

{-
pattern SQL_ATTR_NAME_DBC_INFO_TOKEN :: AttrName
pattern SQL_ATTR_NAME_DBC_INFO_TOKEN <-
  (attrNameEq [C.pure| long {SQL_ATTR_DBC_INFO_TOKEN} |] -> True) where
  SQL_ATTR_NAME_DBC_INFO_TOKEN = AttrName [C.pure| long {SQL_ATTR_DBC_INFO_TOKEN} |] 
-}

pattern SQL_ATTR_NAME_ENLIST_IN_DTC :: AttrName
pattern SQL_ATTR_NAME_ENLIST_IN_DTC <-
  (attrNameEq [C.pure| long {SQL_ATTR_ENLIST_IN_DTC} |] -> True) where
  SQL_ATTR_NAME_ENLIST_IN_DTC = AttrName [C.pure| long {SQL_ATTR_ENLIST_IN_DTC} |] 

pattern SQL_ATTR_NAME_LOGIN_TIMEOUT :: AttrName
pattern SQL_ATTR_NAME_LOGIN_TIMEOUT <-
  (attrNameEq [C.pure| long {SQL_ATTR_LOGIN_TIMEOUT} |] -> True) where
  SQL_ATTR_NAME_LOGIN_TIMEOUT = AttrName [C.pure| long {SQL_ATTR_LOGIN_TIMEOUT} |] 

pattern SQL_ATTR_NAME_METADATA_ID :: AttrName
pattern SQL_ATTR_NAME_METADATA_ID <-
  (attrNameEq [C.pure| long {SQL_ATTR_METADATA_ID} |] -> True) where
  SQL_ATTR_NAME_METADATA_ID = AttrName [C.pure| long {SQL_ATTR_METADATA_ID} |] 

pattern SQL_ATTR_NAME_ODBC_CURSORS :: AttrName
pattern SQL_ATTR_NAME_ODBC_CURSORS <-
  (attrNameEq [C.pure| long {SQL_ATTR_ODBC_CURSORS} |] -> True) where
  SQL_ATTR_NAME_ODBC_CURSORS = AttrName [C.pure| long {SQL_ATTR_ODBC_CURSORS} |] 

pattern SQL_ATTR_NAME_PACKET_SIZE :: AttrName
pattern SQL_ATTR_NAME_PACKET_SIZE <-
  (attrNameEq [C.pure| long {SQL_ATTR_PACKET_SIZE} |] -> True) where
  SQL_ATTR_NAME_PACKET_SIZE = AttrName [C.pure| long {SQL_ATTR_PACKET_SIZE} |] 

pattern SQL_ATTR_NAME_QUIET_MODE :: AttrName
pattern SQL_ATTR_NAME_QUIET_MODE <-
  (attrNameEq [C.pure| long {SQL_ATTR_QUIET_MODE} |] -> True) where
  SQL_ATTR_NAME_QUIET_MODE = AttrName [C.pure| long {SQL_ATTR_QUIET_MODE} |] 

pattern SQL_ATTR_NAME_TRACE :: AttrName
pattern SQL_ATTR_NAME_TRACE <-
  (attrNameEq [C.pure| long {SQL_ATTR_TRACE} |] -> True) where
  SQL_ATTR_NAME_TRACE = AttrName [C.pure| long {SQL_ATTR_TRACE} |] 

pattern SQL_ATTR_NAME_TRACEFILE :: AttrName
pattern SQL_ATTR_NAME_TRACEFILE <-
  (attrNameEq [C.pure| long {SQL_ATTR_TRACEFILE} |] -> True) where
  SQL_ATTR_NAME_TRACEFILE = AttrName [C.pure| long {SQL_ATTR_TRACEFILE} |] 

pattern SQL_ATTR_NAME_TRANSLATE_LIB :: AttrName
pattern SQL_ATTR_NAME_TRANSLATE_LIB <-
  (attrNameEq [C.pure| long {SQL_ATTR_TRANSLATE_LIB} |] -> True) where
  SQL_ATTR_NAME_TRANSLATE_LIB = AttrName [C.pure| long {SQL_ATTR_TRANSLATE_LIB} |] 

pattern SQL_ATTR_NAME_TRANSLATE_OPTION :: AttrName
pattern SQL_ATTR_NAME_TRANSLATE_OPTION <-
  (attrNameEq [C.pure| long {SQL_ATTR_TRANSLATE_OPTION} |] -> True) where
  SQL_ATTR_NAME_TRANSLATE_OPTION = AttrName [C.pure| long {SQL_ATTR_TRANSLATE_OPTION} |] 

pattern SQL_ATTR_NAME_TXN_ISOLATION :: AttrName
pattern SQL_ATTR_NAME_TXN_ISOLATION <-
  (attrNameEq [C.pure| long {SQL_ATTR_TXN_ISOLATION} |] -> True) where
  SQL_ATTR_NAME_TXN_ISOLATION = AttrName [C.pure| long {SQL_ATTR_TXN_ISOLATION} |] 

#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE
   SQL_ATTR_NAME_ACCESS_MODE
 , SQL_ATTR_NAME_ASYNC_DBC_FUNCTIONS_ENABLE
 , SQL_ATTR_NAME_ASYNC_ENABLE
 , SQL_ATTR_NAME_AUTO_IPD
 , SQL_ATTR_NAME_AUTOCOMMIT
 , SQL_ATTR_NAME_CONNECTION_DEAD
 , SQL_ATTR_NAME_CONNECTION_TIMEOUT
 , SQL_ATTR_NAME_CURRENT_CATALOG
 , SQL_ATTR_NAME_ENLIST_IN_DTC
 , SQL_ATTR_NAME_LOGIN_TIMEOUT
 , SQL_ATTR_NAME_METADATA_ID
 , SQL_ATTR_NAME_ODBC_CURSORS
 , SQL_ATTR_NAME_PACKET_SIZE
 , SQL_ATTR_NAME_QUIET_MODE
 , SQL_ATTR_NAME_TRACE
 , SQL_ATTR_NAME_TRACEFILE
 , SQL_ATTR_NAME_TRANSLATE_LIB
 , SQL_ATTR_NAME_TRANSLATE_OPTION
 , SQL_ATTR_NAME_TXN_ISOLATION
 :: AttrName
 #-}
#endif

isFixedSizeAttrValue :: AttrName -> Bool
isFixedSizeAttrValue name
  | name `elem` fixedSizeAttrs = True
  | otherwise                  = False

fixedSizeAttrs :: [AttrName]
fixedSizeAttrs =
  [ SQL_ATTR_NAME_ACCESS_MODE
  , SQL_ATTR_NAME_ASYNC_DBC_FUNCTIONS_ENABLE
  , SQL_ATTR_NAME_ASYNC_ENABLE
  , SQL_ATTR_NAME_AUTO_IPD
  , SQL_ATTR_NAME_AUTOCOMMIT
  , SQL_ATTR_NAME_CONNECTION_DEAD
  , SQL_ATTR_NAME_CONNECTION_TIMEOUT
  , SQL_ATTR_NAME_LOGIN_TIMEOUT
  , SQL_ATTR_NAME_METADATA_ID
  , SQL_ATTR_NAME_ODBC_CURSORS  
  , SQL_ATTR_NAME_PACKET_SIZE
  , SQL_ATTR_NAME_TRACE
  ]

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

attrWithStringMaybePat :: AttrName -> ConnectAttr a -> Maybe ByteString
attrWithStringMaybePat a attr =
  if a == getConnectAttrName attr
  then attrPtrString =<< attrPtr (attrValuePtr attr)
  else Nothing

attrWithBytesMaybePat :: AttrName -> ConnectAttr a -> Maybe ByteString
attrWithBytesMaybePat a attr =
  if a == getConnectAttrName attr
  then attrPtrBytes =<< attrPtr (attrValuePtr attr)
  else Nothing

attrWithValueMaybePat :: AttrName -> ConnectAttr a -> Maybe CULong
attrWithValueMaybePat a attr =
  if a == getConnectAttrName attr
  then attrValue (attrValuePtr attr)
  else Nothing

attrWithValuePat :: AttrName -> CULong -> ConnectAttr a -> Bool
attrWithValuePat a v attr = attrWithValue a v == attr

attrWithValuePtr :: AttrName -> ForeignPtr () -> IO (ConnectAttr a)
attrWithValuePtr a ptr = do
  v <- withForeignPtr (castForeignPtr ptr) peek
  pure (attrWithValue a v)

attrWithValue :: AttrName -> CULong -> ConnectAttr a
attrWithValue a v = ConnectAttr a (AttrValue v)

attrWithStringPtr :: AttrName -> ForeignPtr () -> ForeignPtr CLong -> IO (ConnectAttr a)
attrWithStringPtr a fptr ptrLen = do
  -- NOTE: Validate this
  len <- withForeignPtr ptrLen peek
  v <- withForeignPtr (castForeignPtr fptr) (\ptr -> packCStringLen (ptr, fromIntegral len))
  pure (attrWithString a v)

attrWithString :: AttrName -> ByteString -> ConnectAttr a
attrWithString a v = ConnectAttr a (AttrPtr (String v))

attrWithBytesPtr :: AttrName -> ForeignPtr () -> ForeignPtr CLong -> IO (ConnectAttr a)
attrWithBytesPtr a fptr ptrLen = do
  -- NOTE: Validate this
  len <- withForeignPtr ptrLen peek
  v <- withForeignPtr (castForeignPtr fptr) (\ptr -> packCStringLen (ptr, fromIntegral len))
  pure (attrWithBytes a v)

attrWithBytes :: AttrName -> ByteString -> ConnectAttr a
attrWithBytes a v = ConnectAttr a (AttrPtr (Bytes v))

pattern SQL_ATTR_ACCESS_MODE_READ_ONLY :: ConnectAttr a
pattern SQL_ATTR_ACCESS_MODE_READ_ONLY <-
  (attrWithValuePat SQL_ATTR_NAME_ACCESS_MODE [C.pure| SQLUINTEGER { SQL_MODE_READ_ONLY } |] -> True) where
  SQL_ATTR_ACCESS_MODE_READ_ONLY = attrWithValue SQL_ATTR_NAME_ACCESS_MODE [C.pure| SQLUINTEGER { SQL_MODE_READ_ONLY } |]

pattern SQL_ATTR_ACCESS_MODE_READ_WRITE :: ConnectAttr a
pattern SQL_ATTR_ACCESS_MODE_READ_WRITE <-
  (attrWithValuePat SQL_ATTR_NAME_ACCESS_MODE [C.pure| SQLUINTEGER { SQL_MODE_READ_WRITE } |] -> True) where
  SQL_ATTR_ACCESS_MODE_READ_WRITE = attrWithValue SQL_ATTR_NAME_ACCESS_MODE [C.pure| SQLUINTEGER { SQL_MODE_READ_WRITE } |]

-- SQL_ATTR_ASYNC_DBC_EVENT
-- SQL_ATTR_DBC_FUNCTIONS_ENABLE
-- SQL_ATTR_ASYNC_DBC_PCALLBACK [Only driver manager can call]
-- SQL_ATTR_ASYNC_DBC_PCONTEXT  [Only driver manager can call]

pattern SQL_ATTR_ASYNC_ENABLE_ON :: ConnectAttr a
pattern SQL_ATTR_ASYNC_ENABLE_ON <-
  (attrWithValuePat SQL_ATTR_NAME_ASYNC_ENABLE [C.pure| SQLULEN {SQL_ASYNC_ENABLE_ON} |] -> True) where
  SQL_ATTR_ASYNC_ENABLE_ON = attrWithValue SQL_ATTR_NAME_ASYNC_ENABLE [C.pure| SQLUINTEGER {SQL_ASYNC_ENABLE_ON} |]

pattern SQL_ATTR_ASYNC_ENABLE_OFF :: ConnectAttr a
pattern SQL_ATTR_ASYNC_ENABLE_OFF <-
  (attrWithValuePat SQL_ATTR_NAME_ASYNC_ENABLE [C.pure| SQLULEN {SQL_ASYNC_ENABLE_OFF} |] -> True) where
  SQL_ATTR_ASYNC_ENABLE_OFF = attrWithValue SQL_ATTR_NAME_ASYNC_ENABLE [C.pure| SQLUINTEGER {SQL_ASYNC_ENABLE_OFF} |]

-- SQL_ATTR_AUTO_IPD cannot be set by SQLSetConnectAttr
-- should SQL_ATTR_AUTOCOMMIT be exposed?
-- SQL_ATTR_CONNECTION_DEAD read only

pattern SQL_ATTR_CONNECTION_TIMEOUT :: CULong -> ConnectAttr a
pattern SQL_ATTR_CONNECTION_TIMEOUT i <-
  (attrWithValueMaybePat SQL_ATTR_NAME_CONNECTION_TIMEOUT -> Just i) where
  SQL_ATTR_CONNECTION_TIMEOUT i = attrWithValue SQL_ATTR_NAME_CONNECTION_TIMEOUT i

pattern SQL_ATTR_CURRENT_CATALOG :: ByteString -> ConnectAttr a
pattern SQL_ATTR_CURRENT_CATALOG i <-
  (attrWithStringMaybePat SQL_ATTR_NAME_CURRENT_CATALOG -> Just i) where
  SQL_ATTR_CURRENT_CATALOG i = attrWithString SQL_ATTR_NAME_CURRENT_CATALOG i

-- SQL_DBC_INFO_TOKEN 
-- SQL_ATTR_ENLIST_IN_DTC

pattern SQL_ATTR_LOGIN_TIMEOUT :: CULong -> ConnectAttr 'ConnectBefore
pattern SQL_ATTR_LOGIN_TIMEOUT i <-
  (attrWithValueMaybePat SQL_ATTR_NAME_LOGIN_TIMEOUT -> Just i) where
  SQL_ATTR_LOGIN_TIMEOUT i = attrWithValue SQL_ATTR_NAME_LOGIN_TIMEOUT i

pattern SQL_ATTR_METADATA_ID :: CULong -> ConnectAttr a
pattern SQL_ATTR_METADATA_ID i <-
  (attrWithValueMaybePat SQL_ATTR_NAME_METADATA_ID -> Just i) where
  SQL_ATTR_METADATA_ID i = attrWithValue SQL_ATTR_NAME_METADATA_ID i

pattern SQL_ATTR_ODBC_CURSORS_CUR_USE_IF_NEEDED :: ConnectAttr 'ConnectBefore
pattern SQL_ATTR_ODBC_CURSORS_CUR_USE_IF_NEEDED <-
  (attrWithValuePat SQL_ATTR_NAME_ODBC_CURSORS [C.pure| SQLULEN { SQL_CUR_USE_IF_NEEDED } |] -> True) where
  SQL_ATTR_ODBC_CURSORS_CUR_USE_IF_NEEDED = attrWithValue SQL_ATTR_NAME_ODBC_CURSORS [C.pure| SQLULEN { SQL_CUR_USE_IF_NEEDED } |]

pattern SQL_ATTR_ODBC_CURSORS_CUR_USE_ODBC :: ConnectAttr 'ConnectBefore
pattern SQL_ATTR_ODBC_CURSORS_CUR_USE_ODBC <-
  (attrWithValuePat SQL_ATTR_NAME_ODBC_CURSORS [C.pure| SQLULEN { SQL_CUR_USE_ODBC } |] -> True) where
  SQL_ATTR_ODBC_CURSORS_CUR_USE_ODBC = attrWithValue SQL_ATTR_NAME_ODBC_CURSORS [C.pure| SQLULEN { SQL_CUR_USE_ODBC } |]

pattern SQL_ATTR_ODBC_CURSORS_CUR_USE_DRIVER :: ConnectAttr 'ConnectBefore
pattern SQL_ATTR_ODBC_CURSORS_CUR_USE_DRIVER <-
  (attrWithValuePat SQL_ATTR_NAME_ODBC_CURSORS [C.pure| SQLULEN { SQL_CUR_USE_DRIVER } |] -> True) where
  SQL_ATTR_ODBC_CURSORS_CUR_USE_DRIVER = attrWithValue SQL_ATTR_NAME_ODBC_CURSORS [C.pure| SQLULEN { SQL_CUR_USE_DRIVER } |]

pattern SQL_ATTR_PACKET_SIZE :: CULong -> ConnectAttr 'ConnectBefore
pattern SQL_ATTR_PACKET_SIZE i <-
  (attrWithValueMaybePat SQL_ATTR_NAME_PACKET_SIZE -> Just i) where
  SQL_ATTR_PACKET_SIZE i = attrWithValue SQL_ATTR_NAME_PACKET_SIZE i

-- SQL_ATTR_QUIET_MODE

pattern SQL_ATTR_TRACE_ON :: ConnectAttr a
pattern SQL_ATTR_TRACE_ON <-
  (attrWithValuePat SQL_ATTR_NAME_TRACE [C.pure| SQLUINTEGER { SQL_OPT_TRACE_ON } |] -> True) where
  SQL_ATTR_TRACE_ON = attrWithValue SQL_ATTR_NAME_TRACE [C.pure| SQLUINTEGER { SQL_OPT_TRACE_ON } |]

pattern SQL_ATTR_TRACE_OFF :: ConnectAttr a
pattern SQL_ATTR_TRACE_OFF <-
  (attrWithValuePat SQL_ATTR_NAME_TRACE [C.pure| SQLUINTEGER { SQL_OPT_TRACE_OFF } |] -> True) where
  SQL_ATTR_TRACE_OFF = attrWithValue SQL_ATTR_NAME_TRACE [C.pure| SQLUINTEGER { SQL_OPT_TRACE_OFF } |]

pattern SQL_ATTR_TRACEFILE :: ByteString -> ConnectAttr a
pattern SQL_ATTR_TRACEFILE i <-
  (attrWithStringMaybePat SQL_ATTR_NAME_TRACEFILE -> Just i) where
  SQL_ATTR_TRACEFILE i = attrWithString SQL_ATTR_NAME_TRACEFILE i

pattern SQL_ATTR_TRANSLATE_LIB :: ByteString -> ConnectAttr 'ConnectAfter
pattern SQL_ATTR_TRANSLATE_LIB i <-
  (attrWithStringMaybePat SQL_ATTR_NAME_TRANSLATE_LIB -> Just i) where
  SQL_ATTR_TRANSLATE_LIB i = attrWithString SQL_ATTR_NAME_TRANSLATE_LIB i

pattern SQL_ATTR_TRANSLATE_OPTION :: ByteString -> ConnectAttr 'ConnectAfter
pattern SQL_ATTR_TRANSLATE_OPTION i <-
  (attrWithBytesMaybePat SQL_ATTR_NAME_TRANSLATE_OPTION -> Just i) where
  SQL_ATTR_TRANSLATE_OPTION i = attrWithBytes SQL_ATTR_NAME_TRANSLATE_OPTION i

pattern SQL_ATTR_TXN_ISOLATION :: ByteString -> ConnectAttr a
pattern SQL_ATTR_TXN_ISOLATION i <-
  (attrWithBytesMaybePat SQL_ATTR_NAME_TXN_ISOLATION -> Just i) where
  SQL_ATTR_TXN_ISOLATION i = attrWithBytes SQL_ATTR_NAME_TXN_ISOLATION i

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
