{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ViewPatterns               #-}

module Database.MSSQL.Internal.Types where

import qualified Data.Text as T
import Data.Word
import Data.String
import Control.Exception
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Foreign.C
import Foreign.Ptr
import GHC.Generics
import Control.Monad.IO.Class
import Database.MSSQL.Internal.SQLError
import Data.ByteString
import Foreign.Storable
import Foreign.ForeignPtr
import qualified Language.C.Inline as C
import Database.MSSQL.Internal.SQLTypes
import Database.MSSQL.Internal.Ctx
import Data.UUID.Types (UUID)

data SQLHENV
data SQLHDBC
data SQLHSTMT
data SQLHANDLE

newtype ColPos = ColPos (IORef CUShort)

data HSTMT a = HSTMT
  { getHSTMT :: Ptr SQLHSTMT
  , colPos   :: ColPos
  , numResultCols :: CShort
  } deriving Functor

data Connection = Connection
  { _henv :: Ptr SQLHENV
  , _hdbc :: Ptr SQLHDBC
  
  } deriving (Show)

C.context $ mssqlCtx
  [ ("SQLWCHAR", [t|CWchar|])
  , ("SQLCHAR", [t|CChar|]) 
  , ("SQLHANDLE", [t|Ptr SQLHANDLE|])
  , ("SQLHENV" , [t|Ptr SQLHENV|])
  , ("SQLHDBC" , [t|Ptr SQLHDBC|])
  , ("SQLHSTMT" , [t|Ptr SQLHSTMT|])
  , ("SQLPOINTER" , [t|Ptr ()|])  
  , ("SQLSMALLINT", [t|CShort|])
  , ("SQLUSMALLINT", [t|CUShort|])
  , ("SQLREAL", [t|CFloat|])
  , ("SQLFLOAT", [t|CDouble|])
  , ("SQLDOUBLE", [t|CDouble|])
  , ("SQLUINTEGER", [t|CULong|])
  , ("SQLINTEGER", [t|CLong|])
  , ("SQLLEN", [t|CLong|])
  , ("SQLULEN", [t|CULong|])
  , ("SQL_DATE_STRUCT", [t|CDate|])
  , ("SQL_TIME_STRUCT", [t|CTimeOfDay|])
  , ("SQL_SS_TIME2_STRUCT", [t|CTimeOfDay|])
  , ("SQL_TIMESTAMP_STRUCT", [t|CLocalTime|])
  , ("SQL_SS_TIMESTAMPOFFSET_STRUCT", [t|CZonedTime|])
  , ("SQLGUID", [t|UUID|])
  ]

C.verbatim "#define UNICODE"

#ifdef mingw32_HOST_OS
C.include "<windows.h>"
#endif
C.include "<stdio.h>"
C.include "<stdlib.h>"
C.include "<sqlext.h>"
C.include "<sqltypes.h>"
C.include "<sqlucode.h>"
C.include "<ss.h>"

data ConnectParams = ConnectParams
                     T.Text
                     T.Text
                     T.Text
                     T.Text
                     Word16
                     OdbcDriver
                     Properties
                     deriving Show

data OdbcDriver = OdbcSQLServer Word8
                | OtherOdbcDriver T.Text
                deriving Show

odbcSQLServer17 :: OdbcDriver
odbcSQLServer17 = OdbcSQLServer 17

odbcSQLServer12 :: OdbcDriver
odbcSQLServer12 = OdbcSQLServer 12

ppOdbcDriver :: OdbcDriver -> T.Text
ppOdbcDriver dv = case dv of
  OdbcSQLServer v   -> "Driver={ODBC Driver " <> T.pack (show v) <> " for SQL Server};"
  OtherOdbcDriver t -> "Driver=" <> t <> ";"

ppConnectionString :: ConnectionString -> T.Text
ppConnectionString (ConnectionString' (Left str)) = str
ppConnectionString (ConnectionString' (Right (ConnectParams db ser pass usr pt dv cp))) =
  let ppDriver    = ppOdbcDriver dv
      ppServer    = ser
      ppDb        = db
      ppPass      = pass
      ppUser      = usr
      ppPort      = tshow pt
      
  in ppDriver <>
     "Server=" <> ppServer <> "," <> ppPort <> ";" <>
     "Database=" <> ppDb <> ";" <>
     "UID="<> ppUser <>";PWD=" <> ppPass <>";" <>
     ppProps cp

  where tshow = T.pack . show
        ppProps = HM.foldlWithKey' (\k v ac -> k <> "=" <> v <> ";" <> ac) "" 


newtype ConnectionString = ConnectionString' { getConString :: Either T.Text ConnectParams }
                         deriving (Show)

type Properties = HM.HashMap T.Text T.Text

defProperties :: Properties
defProperties = HM.empty

pattern ConnectionString :: T.Text -> T.Text -> Word16 -> T.Text -> T.Text -> OdbcDriver -> Properties -> ConnectionString
pattern ConnectionString { database, server, port, user, password, odbcDriver, connectProperties } =
  ConnectionString' (Right (ConnectParams database server password user port odbcDriver connectProperties))
  
instance IsString ConnectionString where
  fromString = ConnectionString' . Left . T.pack

data ConnectInfo = ConnectInfo
  { connectionString :: ConnectionString
  , attrBefore :: [ConnectAttr 'ConnectBefore]
  , attrAfter  :: [ConnectAttr 'ConnectAfter]
  }

data Config = Config
  { boundSizeLimit :: Word
  , bufferSize :: Word
  } deriving (Show, Eq)

defConfig :: Config
defConfig = Config { boundSizeLimit = _64Kb, bufferSize = _64Kb }

_64Kb :: Word
_64Kb = 65536

data SQLNumResultColsException = SQLNumResultColsException { expected :: CShort, actual :: CShort }
                              deriving (Generic)

instance Exception SQLNumResultColsException

instance Show SQLNumResultColsException where
  show (SQLNumResultColsException e a) =
    "Mismatch between expected column count and actual query column count. Expected column count is " <>
    show e <> ", but actual query column count is " <> show a

data SQLColumnSizeException = SQLColumnSizeException Integer Integer
                              deriving (Generic)

instance Exception SQLColumnSizeException

instance Show SQLColumnSizeException where
  show (SQLColumnSizeException e a) =
    "Mismatch between expected column size in type and actual column size in database. Expected column size in type is " <>
    show e <> ", but actual column size in database is " <> show a


data SQLException = SQLException {getSQLErrors :: SQLErrors }
                  deriving (Show, Generic)

instance Exception SQLException

throwSQLException :: (MonadIO m) => SQLErrors -> m a
throwSQLException = liftIO . throwIO . SQLException

data HandleRef
  = SQLENVRef (Ptr SQLHENV)
  | SQLDBCRef (Ptr SQLHDBC)
  | SQLSTMTRef (Ptr SQLHSTMT)
  
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
            
data ColBufferTypeK =
    BindCol
  | GetDataBound
  | GetDataUnbound
  
data ColBufferType (k :: ColBufferTypeK) t where
  BindColBuffer :: ForeignPtr CLong -> ForeignPtr t -> ColBufferType 'BindCol t
  GetDataBoundBuffer :: IO (ForeignPtr t, ForeignPtr CLong) -> ColBufferType 'GetDataBound t
  GetDataUnboundBuffer :: (forall a. a -> (CLong -> CLong -> Ptr t -> a -> IO a) -> IO a) -> ColBufferType 'GetDataUnbound t

data ColBufferSizeK =
    Unbounded
  | Bounded

type family GetColBufferTypeFromSize (tySz :: ColBufferSizeK) :: ColBufferTypeK where
  GetColBufferTypeFromSize 'Unbounded = 'GetDataUnbound
  GetColBufferTypeFromSize 'Bounded   = 'GetDataBound  

data ColDescriptor = ColDescriptor
  { colName         :: T.Text
  , colDataType     :: SQLType
  , colSize         :: Word
  , colDecimalDigit :: Int
  , colIsNullable   :: Maybe Bool
  , colPosition     :: CUShort
  } deriving (Show, Eq)

newtype SQLType = SQLType C.CShort
  deriving (Eq, Storable)

instance Show SQLType where
  show = \case
    SQL_UNKNOWN_TYPE       -> "SQL_UNKNOWN_TYPE"
    SQL_CHAR               -> "SQL_CHAR"
    SQL_NUMERIC            -> "SQL_NUMERIC"
    SQL_DECIMAL            -> "SQL_DECIMAL"
    SQL_INTEGER            -> "SQL_INTEGER"
    SQL_SMALLINT           -> "SQL_SMALLINT"
    SQL_REAL               -> "SQL_REAL"
    SQL_FLOAT              -> "SQL_FLOAT"
    SQL_DOUBLE             -> "SQL_DOUBLE"
    SQL_DATETIME           -> "SQL_DATETIME"
    SQL_VARCHAR            -> "SQL_VARCHAR"
    SQL_DATE               -> "SQL_DATE"
    SQL_TYPE_DATE          -> "SQL_TYPE_DATE"
    SQL_INTERVAL           -> "SQL_INTERVAL"
    SQL_TIME               -> "SQL_TIME"
    SQL_TIMESTAMP          -> "SQL_TIMESTAMP"
    SQL_TYPE_TIMESTAMP     -> "SQL_TYPE_TIMESTAMP"
    SQL_LONGVARCHAR        -> "SQL_LONGVARCHAR"
    SQL_BINARY             -> "SQL_BINARY"
    SQL_VARBINARY          -> "SQL_VARBINARY"
    SQL_LONGVARBINARY      -> "SQL_LONGVARBINARY"
    SQL_BIGINT             -> "SQL_BIGINT"
    SQL_TINYINT            -> "SQL_TINYINT"
    SQL_BIT                -> "SQL_BIT"
    SQL_GUID               -> "SQL_GUID"
    SQL_WCHAR              -> "SQL_WCHAR"
    SQL_WVARCHAR           -> "SQL_WVARCHAR"
    SQL_WLONGVARCHAR       -> "SQL_WLONGVARCHAR"
    SQL_SS_TIME2           -> "SQL_SS_TIME2"
    SQL_SS_TIMESTAMPOFFSET -> "SQL_SS_TIMESTAMPOFFSET"
#if __GLASGOW_HASKELL__ < 802
    _                    -> error "Panic: impossible case"
#endif

pattern SQL_UNKNOWN_TYPE :: SQLType
pattern SQL_UNKNOWN_TYPE <- ((SQLType [C.pure| SQLSMALLINT {SQL_UNKNOWN_TYPE} |] ==) -> True) where
  SQL_UNKNOWN_TYPE = SQLType [C.pure| SQLSMALLINT {SQL_UNKNOWN_TYPE} |]

pattern SQL_CHAR :: SQLType
pattern SQL_CHAR <- ((SQLType [C.pure| SQLSMALLINT {SQL_CHAR} |] ==) -> True) where
  SQL_CHAR = SQLType [C.pure| SQLSMALLINT {SQL_CHAR} |]

pattern SQL_NUMERIC :: SQLType
pattern SQL_NUMERIC <- ((SQLType [C.pure| SQLSMALLINT {SQL_NUMERIC} |] ==) -> True) where
  SQL_NUMERIC = SQLType [C.pure| SQLSMALLINT {SQL_NUMERIC} |]

pattern SQL_DECIMAL :: SQLType
pattern SQL_DECIMAL <- ((SQLType [C.pure| SQLSMALLINT {SQL_DECIMAL} |] ==) -> True) where
  SQL_DECIMAL = SQLType [C.pure| SQLSMALLINT {SQL_DECIMAL} |]  

pattern SQL_INTEGER :: SQLType
pattern SQL_INTEGER <- ((SQLType [C.pure| SQLSMALLINT {SQL_INTEGER} |] ==) -> True) where
  SQL_INTEGER = SQLType [C.pure| SQLSMALLINT {SQL_INTEGER} |]

pattern SQL_SMALLINT :: SQLType
pattern SQL_SMALLINT <- ((SQLType [C.pure| SQLSMALLINT {SQL_SMALLINT} |] ==) -> True) where
  SQL_SMALLINT = SQLType [C.pure| SQLSMALLINT {SQL_SMALLINT} |]

pattern SQL_FLOAT :: SQLType
pattern SQL_FLOAT <- ((SQLType [C.pure| SQLSMALLINT {SQL_FLOAT} |] ==) -> True) where
  SQL_FLOAT = SQLType [C.pure| SQLSMALLINT {SQL_FLOAT} |]

pattern SQL_REAL :: SQLType
pattern SQL_REAL <- ((SQLType [C.pure| SQLSMALLINT {SQL_REAL} |] ==) -> True) where
  SQL_REAL = SQLType [C.pure| SQLSMALLINT {SQL_REAL} |]

pattern SQL_DOUBLE :: SQLType
pattern SQL_DOUBLE <- ((SQLType [C.pure| SQLSMALLINT {SQL_DOUBLE} |] ==) -> True) where
  SQL_DOUBLE = SQLType [C.pure| SQLSMALLINT {SQL_DOUBLE} |]

pattern SQL_DATETIME :: SQLType
pattern SQL_DATETIME <- ((SQLType [C.pure| SQLSMALLINT {SQL_DATETIME} |] ==) -> True) where
  SQL_DATETIME = SQLType [C.pure| SQLSMALLINT {SQL_DATETIME} |]

pattern SQL_VARCHAR :: SQLType
pattern SQL_VARCHAR <- ((SQLType [C.pure| SQLSMALLINT {SQL_VARCHAR} |] ==) -> True) where
  SQL_VARCHAR = SQLType [C.pure| SQLSMALLINT {SQL_VARCHAR} |]

pattern SQL_DATE :: SQLType
pattern SQL_DATE <- ((SQLType [C.pure| SQLSMALLINT {SQL_DATE} |] ==) -> True) where
  SQL_DATE = SQLType [C.pure| SQLSMALLINT {SQL_DATE} |]

pattern SQL_TYPE_DATE :: SQLType
pattern SQL_TYPE_DATE <- ((SQLType [C.pure| SQLSMALLINT {SQL_TYPE_DATE} |] ==) -> True) where
  SQL_TYPE_DATE = SQLType [C.pure| SQLSMALLINT {SQL_TYPE_DATE} |]  

pattern SQL_INTERVAL :: SQLType
pattern SQL_INTERVAL <- ((SQLType [C.pure| SQLSMALLINT {SQL_INTERVAL} |] ==) -> True) where
  SQL_INTERVAL = SQLType [C.pure| SQLSMALLINT {SQL_INTERVAL} |]

pattern SQL_TIME :: SQLType
pattern SQL_TIME <- ((SQLType [C.pure| SQLSMALLINT {SQL_TIME} |] ==) -> True) where
  SQL_TIME = SQLType [C.pure| SQLSMALLINT {SQL_TIME} |]

pattern SQL_TIMESTAMP :: SQLType
pattern SQL_TIMESTAMP <- ((SQLType [C.pure| SQLSMALLINT {SQL_TIMESTAMP} |] ==) -> True) where
  SQL_TIMESTAMP = SQLType [C.pure| SQLSMALLINT {SQL_TIMESTAMP} |]
  
pattern SQL_TYPE_TIMESTAMP :: SQLType
pattern SQL_TYPE_TIMESTAMP <- ((SQLType [C.pure| SQLSMALLINT {SQL_TYPE_TIMESTAMP} |] ==) -> True) where
  SQL_TYPE_TIMESTAMP = SQLType [C.pure| SQLSMALLINT {SQL_TYPE_TIMESTAMP} |]

pattern SQL_LONGVARCHAR :: SQLType
pattern SQL_LONGVARCHAR <- ((SQLType [C.pure| SQLSMALLINT {SQL_LONGVARCHAR} |] ==) -> True) where
  SQL_LONGVARCHAR = SQLType [C.pure| SQLSMALLINT {SQL_LONGVARCHAR} |]

pattern SQL_BINARY :: SQLType
pattern SQL_BINARY <- ((SQLType [C.pure| SQLSMALLINT {SQL_BINARY} |] ==) -> True) where
  SQL_BINARY = SQLType [C.pure| SQLSMALLINT {SQL_BINARY} |]

pattern SQL_VARBINARY :: SQLType
pattern SQL_VARBINARY <- ((SQLType [C.pure| SQLSMALLINT {SQL_VARBINARY} |] ==) -> True) where
  SQL_VARBINARY = SQLType [C.pure| SQLSMALLINT {SQL_VARBINARY} |]

pattern SQL_LONGVARBINARY :: SQLType
pattern SQL_LONGVARBINARY <- ((SQLType [C.pure| SQLSMALLINT {SQL_LONGVARBINARY} |] ==) -> True) where
  SQL_LONGVARBINARY = SQLType [C.pure| SQLSMALLINT {SQL_LONGVARBINARY} |]

pattern SQL_BIGINT :: SQLType
pattern SQL_BIGINT <- ((SQLType [C.pure| SQLSMALLINT {SQL_BIGINT} |] ==) -> True) where
  SQL_BIGINT = SQLType [C.pure| SQLSMALLINT {SQL_BIGINT} |]

pattern SQL_TINYINT :: SQLType
pattern SQL_TINYINT <- ((SQLType [C.pure| SQLSMALLINT {SQL_TINYINT} |] ==) -> True) where
  SQL_TINYINT = SQLType [C.pure| SQLSMALLINT {SQL_TINYINT} |]

pattern SQL_BIT :: SQLType
pattern SQL_BIT <- ((SQLType [C.pure| SQLSMALLINT {SQL_BIT} |] ==) -> True) where
  SQL_BIT = SQLType [C.pure| SQLSMALLINT {SQL_BIT} |]

pattern SQL_GUID :: SQLType
pattern SQL_GUID <- ((SQLType [C.pure| SQLSMALLINT {SQL_GUID} |] ==) -> True) where
  SQL_GUID = SQLType [C.pure| SQLSMALLINT {SQL_GUID} |]

pattern SQL_WCHAR :: SQLType
pattern SQL_WCHAR <- ((SQLType [C.pure| SQLSMALLINT {SQL_WCHAR} |] ==) -> True) where
  SQL_WCHAR = SQLType [C.pure| SQLSMALLINT {SQL_WCHAR} |]

pattern SQL_WVARCHAR :: SQLType
pattern SQL_WVARCHAR <- ((SQLType [C.pure| SQLSMALLINT {SQL_WVARCHAR} |] ==) -> True) where
  SQL_WVARCHAR = SQLType [C.pure| SQLSMALLINT {SQL_WVARCHAR} |]

pattern SQL_WLONGVARCHAR :: SQLType
pattern SQL_WLONGVARCHAR <- ((SQLType [C.pure| SQLSMALLINT {SQL_WLONGVARCHAR} |] ==) -> True) where
  SQL_WLONGVARCHAR = SQLType [C.pure| SQLSMALLINT {SQL_WLONGVARCHAR} |]

pattern SQL_SS_TIME2 :: SQLType
pattern SQL_SS_TIME2 <- ((SQLType [C.pure| SQLSMALLINT {SQL_SS_TIME2} |] ==) -> True) where
  SQL_SS_TIME2 = SQLType [C.pure| SQLSMALLINT {SQL_SS_TIME2} |]

pattern SQL_SS_TIMESTAMPOFFSET :: SQLType
pattern SQL_SS_TIMESTAMPOFFSET <- ((SQLType [C.pure| SQLSMALLINT {SQL_SS_TIMESTAMPOFFSET} |] ==) -> True) where
  SQL_SS_TIMESTAMPOFFSET = SQLType [C.pure| SQLSMALLINT {SQL_SS_TIMESTAMPOFFSET} |]  

#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE
   SQL_UNKNOWN_TYPE
 , SQL_CHAR
 , SQL_NUMERIC
 , SQL_DECIMAL
 , SQL_INTEGER
 , SQL_SMALLINT
 , SQL_FLOAT
 , SQL_REAL
 , SQL_DOUBLE
 , SQL_DATETIME
 , SQL_VARCHAR
 , SQL_DATE
 , SQL_INTERVAL
 , SQL_TIME
 , SQL_TIMESTAMP
 , SQL_LONGVARCHAR
 , SQL_BINARY
 , SQL_VARBINARY
 , SQL_LONGVARBINARY
 , SQL_BIGINT
 , SQL_TINYINT
 , SQL_BIT
 , SQL_GUID
 , SQL_WCHAR
 , SQL_WVARCHAR
 , SQL_WLONGVARCHAR

 , SQL_SS_TIME2
 , SQL_TYPE_DATE
 , SQL_SS_TIMESTAMPOFFSET
 :: SQLType
 #-}
#endif  

newtype HSCType = HSCType C.CInt
  deriving (Show, Read, Eq, Storable)

pattern SQL_C_CHAR :: HSCType
pattern SQL_C_CHAR <- ((HSCType [C.pure| int {SQL_C_CHAR} |] ==) -> True) where
  SQL_C_CHAR = HSCType [C.pure| int {SQL_C_CHAR} |]

pattern SQL_C_LONG :: HSCType
pattern SQL_C_LONG <- ((HSCType [C.pure| int {SQL_C_LONG} |] ==) -> True) where
  SQL_C_LONG = HSCType [C.pure| int {SQL_C_LONG} |]  

pattern SQL_C_SHORT :: HSCType
pattern SQL_C_SHORT <- ((HSCType [C.pure| int {SQL_C_SHORT} |] ==) -> True) where
  SQL_C_SHORT = HSCType [C.pure| int {SQL_C_SHORT} |]


pattern SQL_C_FLOAT :: HSCType
pattern SQL_C_FLOAT <- ((HSCType [C.pure| int {SQL_C_FLOAT} |] ==) -> True) where
  SQL_C_FLOAT = HSCType [C.pure| int {SQL_C_FLOAT} |]

pattern SQL_C_DOUBLE :: HSCType
pattern SQL_C_DOUBLE <- ((HSCType [C.pure| int {SQL_C_DOUBLE} |] ==) -> True) where
  SQL_C_DOUBLE = HSCType [C.pure| int {SQL_C_DOUBLE} |]

pattern SQL_C_NUMERIC :: HSCType
pattern SQL_C_NUMERIC <- ((HSCType [C.pure| int {SQL_C_NUMERIC} |] ==) -> True) where
  SQL_C_NUMERIC = HSCType [C.pure| int {SQL_C_NUMERIC} |]

pattern SQL_C_DEFAULT :: HSCType
pattern SQL_C_DEFAULT <- ((HSCType [C.pure| int {SQL_C_DEFAULT} |] ==) -> True) where
  SQL_C_DEFAULT = HSCType [C.pure| int {SQL_C_DEFAULT} |]

pattern SQL_C_DATE :: HSCType
pattern SQL_C_DATE <- ((HSCType [C.pure| int {SQL_C_DATE} |] ==) -> True) where
  SQL_C_DATE = HSCType [C.pure| int {SQL_C_DATE} |]

pattern SQL_C_TIME :: HSCType
pattern SQL_C_TIME <- ((HSCType [C.pure| int {SQL_C_TIME} |] ==) -> True) where
  SQL_C_TIME = HSCType [C.pure| int {SQL_C_TIME} |]

pattern SQL_C_TIMESTAMP :: HSCType
pattern SQL_C_TIMESTAMP <- ((HSCType [C.pure| int {SQL_C_TIMESTAMP} |] ==) -> True) where
  SQL_C_TIMESTAMP = HSCType [C.pure| int {SQL_C_TIMESTAMP} |]

pattern SQL_C_WCHAR :: HSCType
pattern SQL_C_WCHAR <- ((HSCType [C.pure| int {SQL_C_WCHAR} |] ==) -> True) where
  SQL_C_WCHAR = HSCType [C.pure| int {SQL_C_WCHAR} |]  

#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE
   SQL_C_CHAR
 , SQL_C_LONG
 , SQL_C_SHORT
 , SQL_C_FLOAT
 , SQL_C_DOUBLE
 , SQL_C_NUMERIC
 , SQL_C_DEFAULT
 , SQL_C_DATE
 , SQL_C_TIME
 , SQL_C_TIMESTAMP
 , SQL_C_WCHAR
 :: HSCType
 #-}
#endif

newtype HandleType = HandleType C.CInt
  deriving (Show, Read, Eq, Storable)

pattern SQL_HANDLE_ENV :: HandleType
pattern SQL_HANDLE_ENV <- ((HandleType [C.pure| int {SQL_HANDLE_ENV} |] ==) -> True) where
  SQL_HANDLE_ENV = HandleType [C.pure| int {SQL_HANDLE_ENV} |]

pattern SQL_HANDLE_DBC :: HandleType
pattern SQL_HANDLE_DBC <- ((HandleType [C.pure| int {SQL_HANDLE_DBC} |] ==) -> True) where
  SQL_HANDLE_DBC = HandleType [C.pure| int {SQL_HANDLE_DBC} |]

pattern SQL_HANDLE_STMT :: HandleType
pattern SQL_HANDLE_STMT <- ((HandleType [C.pure| int {SQL_HANDLE_STMT} |] ==) -> True) where
  SQL_HANDLE_STMT = HandleType [C.pure| int {SQL_HANDLE_STMT} |]

pattern SQL_HANDLE_DESC :: HandleType
pattern SQL_HANDLE_DESC <- ((HandleType [C.pure| int {SQL_HANDLE_DESC} |] ==) -> True) where
  SQL_HANDLE_DESC = HandleType [C.pure| int {SQL_HANDLE_DESC} |]

#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE
   SQL_HANDLE_ENV
 , SQL_HANDLE_DBC
 , SQL_HANDLE_STMT
 , SQL_HANDLE_DESC
 :: HandleType
 #-}
#endif

newtype NullableFieldDesc = NullableFieldDesc C.CShort
  deriving (Show, Read, Eq, Storable)

pattern SQL_NO_NULLS :: NullableFieldDesc
pattern SQL_NO_NULLS <- ((NullableFieldDesc [C.pure| SQLSMALLINT {SQL_NO_NULLS} |] ==) -> True) where
  SQL_NO_NULLS = NullableFieldDesc [C.pure| SQLSMALLINT {SQL_NO_NULLS} |]

pattern SQL_NULLABLE :: NullableFieldDesc
pattern SQL_NULLABLE <- ((NullableFieldDesc [C.pure| SQLSMALLINT {SQL_NULLABLE} |] ==) -> True) where
  SQL_NULLABLE = NullableFieldDesc [C.pure| SQLSMALLINT {SQL_NULLABLE} |]

pattern SQL_NULLABLE_UNKNOWN :: NullableFieldDesc
pattern SQL_NULLABLE_UNKNOWN <- ((NullableFieldDesc [C.pure| SQLSMALLINT {SQL_NULLABLE_UNKNOWN} |] ==) -> True) where
  SQL_NULLABLE_UNKNOWN = NullableFieldDesc [C.pure| SQLSMALLINT {SQL_NULLABLE_UNKNOWN} |]

#if __GLASGOW_HASKELL__ >= 820
{-# COMPLETE
   SQL_NO_NULLS
 , SQL_NULLABLE
 , SQL_NULLABLE_UNKNOWN
 :: NullableFieldDesc
 #-}  
#endif

newtype ResIndicator = ResIndicator C.CInt
  deriving (Show, Read, Eq, Storable, Num, Integral, Real, Enum, Ord)

pattern SQL_NULL_DATA :: ResIndicator
pattern SQL_NULL_DATA <- ((ResIndicator [C.pure| SQLRETURN {SQL_NULL_DATA} |] ==) -> True) where
  SQL_NULL_DATA = ResIndicator [C.pure| SQLRETURN {SQL_NULL_DATA} |]  

pattern SQL_NO_TOTAL :: ResIndicator
pattern SQL_NO_TOTAL <- ((ResIndicator [C.pure| SQLRETURN {SQL_NO_TOTAL} |] ==) -> True) where
  SQL_NO_TOTAL = ResIndicator [C.pure| SQLRETURN {SQL_NO_TOTAL} |]

pattern SQL_DATA_AT_EXEC :: ResIndicator
pattern SQL_DATA_AT_EXEC <- ((ResIndicator [C.pure| SQLRETURN {SQL_DATA_AT_EXEC} |] ==) -> True) where
  SQL_DATA_AT_EXEC = ResIndicator [C.pure| SQLRETURN {SQL_DATA_AT_EXEC} |]

pattern SQL_SUCCESS :: ResIndicator
pattern SQL_SUCCESS <- ((ResIndicator [C.pure| SQLRETURN {SQL_SUCCESS} |] ==) -> True) where
  SQL_SUCCESS = ResIndicator [C.pure| SQLRETURN {SQL_SUCCESS} |]

pattern SQL_SUCCESS_WITH_INFO :: ResIndicator
pattern SQL_SUCCESS_WITH_INFO <- ((ResIndicator [C.pure| SQLRETURN {SQL_SUCCESS_WITH_INFO} |] ==) -> True) where
  SQL_SUCCESS_WITH_INFO = ResIndicator [C.pure| SQLRETURN {SQL_SUCCESS_WITH_INFO} |]

pattern SQL_NO_DATA :: ResIndicator
pattern SQL_NO_DATA <- ((ResIndicator [C.pure| SQLRETURN {SQL_NO_DATA} |] ==) -> True) where
  SQL_NO_DATA = ResIndicator [C.pure| SQLRETURN {SQL_NO_DATA} |]

pattern SQL_ERROR :: ResIndicator
pattern SQL_ERROR <- ((ResIndicator [C.pure| SQLRETURN {SQL_ERROR} |] ==) -> True) where
  SQL_ERROR = ResIndicator [C.pure| SQLRETURN {SQL_ERROR} |]

pattern SQL_INVALID_HANDLE :: ResIndicator
pattern SQL_INVALID_HANDLE <- ((ResIndicator [C.pure| SQLRETURN {SQL_INVALID_HANDLE} |] ==) -> True) where
  SQL_INVALID_HANDLE = ResIndicator [C.pure| SQLRETURN {SQL_INVALID_HANDLE} |]

pattern SQL_STILL_EXECUTING :: ResIndicator
pattern SQL_STILL_EXECUTING <- ((ResIndicator [C.pure| SQLRETURN {SQL_STILL_EXECUTING} |] ==) -> True) where
  SQL_STILL_EXECUTING = ResIndicator [C.pure| SQLRETURN {SQL_STILL_EXECUTING} |]

pattern SQL_NEED_DATA :: ResIndicator
pattern SQL_NEED_DATA <- ((ResIndicator [C.pure| SQLRETURN {SQL_NEED_DATA} |] ==) -> True) where
  SQL_NEED_DATA = ResIndicator [C.pure| SQLRETURN {SQL_NEED_DATA} |]  

#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE
   SQL_NULL_DATA
 , SQL_NO_TOTAL
 , SQL_DATA_AT_EXEC
 , SQL_SUCCESS
 , SQL_SUCCESS_WITH_INFO
 , SQL_NO_DATA
 , SQL_ERROR
 , SQL_INVALID_HANDLE
 , SQL_STILL_EXECUTING
 , SQL_NEED_DATA
 :: ResIndicator
 #-}
#endif

restmt :: forall a b. HSTMT a -> HSTMT b
restmt (HSTMT stm cp ncs) = HSTMT stm cp ncs :: HSTMT b

