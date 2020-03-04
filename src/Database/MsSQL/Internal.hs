{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
module Database.MsSQL.Internal
  ( module Database.MsSQL.Internal.SQLError
  , module Database.MsSQL.Internal
  ) where


import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BSB
import qualified Language.C.Inline as C
import Foreign.Storable
import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc (alloca)
import qualified Data.Vector.Storable as SV
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Coerce
import Database.MsSQL.Internal.Ctx
import Database.MsSQL.Internal.SQLError
import Data.Text.Foreign as T
import qualified Data.Text as T
import Text.Read
import Data.IORef
import Data.Word
import Data.Int
import GHC.Generics
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.IO.Class
import Data.Time
import Data.UUID.Types
import Database.MsSQL.Internal.SQLTypes
import Data.Functor.Identity
import Control.Monad
import Control.Monad.Reader.Class
import Data.String
import qualified Data.HashMap.Strict as HM
#if __GLASGOW_HASKELL__ < 802
import Data.Semigroup
#endif
import Data.Functor.Compose
import Control.Applicative hiding ((<**>))
import Data.Scientific
import Data.Typeable
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy as LT
import GHC.TypeLits

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
  , autoCommit :: Bool
  , ansi :: Bool
  , timeout :: Int
  , readOnly :: Bool
  , attrBefore :: SV.Vector ConnectAttr
  } deriving Show

newtype ConnectAttr = ConnectAttr { getConnectAttr :: C.CInt}
                          deriving (Show, Read, Eq, Storable)

data SQLHENV
data SQLHDBC
data SQLHSTMT
data SQLHANDLE

newtype ColPos = ColPos (IORef CUShort)

data HSTMT a = HSTMT
  { getHSTMT :: Ptr SQLHSTMT
  , colPos   :: ColPos
  } deriving Functor

C.context $ mssqlCtx
  [ ("SQLWCHAR", [t|CWchar|])
  , ("SQLCHAR", [t|CChar|]) 
  , ("SQLHANDLE", [t|Ptr SQLHANDLE|])
  , ("SQLHENV" , [t|Ptr SQLHENV|])
  , ("SQLHDBC" , [t|Ptr SQLHDBC|])
  , ("SQLHSTMT" , [t|Ptr SQLHSTMT|])
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

connectInfo :: ConnectionString -> ConnectInfo
connectInfo conStr = ConnectInfo
  { connectionString = conStr
  , autoCommit = True
  , ansi = False
  , timeout = 5
  , readOnly = False
  , attrBefore = SV.empty
  }

data Connection = Connection
  { _henv :: Ptr SQLHENV
  , _hdbc :: Ptr SQLHDBC
  } deriving (Show)

newtype Session a = Session { getSession :: ReaderT Connection (ExceptT SQLErrors IO) a}
                  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Connection)

throwSQLErr :: SQLErrors -> Session a
throwSQLErr errs = Session $ ReaderT (\_ -> throwE errs)

connect :: ConnectInfo -> IO (Either SQLErrors Connection)
connect connInfo = do
  alloca $ \(henvp :: Ptr (Ptr SQLHENV)) -> do
    alloca $ \(hdbcp :: Ptr (Ptr SQLHDBC)) -> do
      doConnect henvp hdbcp
  where
    doConnect henvp hdbcp = do
      (ctxt, i16) <- asForeignPtr $
        ppConnectionString (connectionString connInfo)
      let ctxtLen = fromIntegral i16 :: C.CInt

      ret <- ResIndicator <$> [C.block| int {
        SQLRETURN ret = 0;
        SQLHENV* henvp = $(SQLHENV* henvp);
        SQLHDBC* hdbcp = $(SQLHDBC* hdbcp);
      
        ret = SQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, henvp);
        if (!SQL_SUCCEEDED(ret)) return ret;

        ret = SQLSetEnvAttr(*henvp, SQL_ATTR_ODBC_VERSION, (void*)SQL_OV_ODBC3, 0);
        if (!SQL_SUCCEEDED(ret)) return ret;

        ret = SQLAllocHandle(SQL_HANDLE_DBC, *henvp, hdbcp);
        if (!SQL_SUCCEEDED(ret)) return ret;

        ret = SQLSetConnectAttr(*hdbcp, SQL_LOGIN_TIMEOUT, (SQLPOINTER)5, 0);
        if (!SQL_SUCCEEDED(ret)) return ret;

        SQLWCHAR* cstr = $fptr-ptr:(SQLWCHAR * ctxt);
        ret = SQLDriverConnectW(*hdbcp, 0, cstr, (SQLSMALLINT)$(int ctxtLen), 0, 0, 0, SQL_DRIVER_NOPROMPT);

        return ret;
        }|]

      henv <- peek henvp
      hdbc <- peek hdbcp
      case isSuccessful ret of
        False -> Left <$> getErrors ret (SQLDBCRef hdbc)
        True -> pure $ Right $ Connection
                           { _henv = henv
                           , _hdbc = hdbc
                           }

disconnect :: Connection -> IO (Either SQLErrors ())
disconnect con = do
  ret <- ResIndicator <$> [C.block| int {
    SQLRETURN ret = 0;
    SQLHENV henv = $(SQLHENV henv);
    SQLHDBC hdbc = $(SQLHDBC hdbc);

    if (hdbc != SQL_NULL_HDBC) {
      ret = SQLDisconnect(hdbc);
      if (!SQL_SUCCEEDED(ret)) return ret;
      
      ret = SQLFreeHandle(SQL_HANDLE_DBC, hdbc);
      if (!SQL_SUCCEEDED(ret)) return ret;
    }

    if (henv != SQL_NULL_HENV)
    {
      ret = SQLFreeHandle(SQL_HANDLE_ENV, henv);
      if (!SQL_SUCCEEDED(ret)) return ret;
    }
    return ret;

  }|]

  case isSuccessful ret of
    True -> pure $ Right ()
    False -> do
      dbErrs <- getErrors ret (SQLDBCRef hdbc)
      envErrs <- getErrors ret (SQLENVRef henv)
      pure $ Left (dbErrs <> envErrs)
  where
    hdbc = _hdbc con
    henv = _henv con

runSession :: ConnectInfo -> Session a -> IO (Either SQLErrors a)
runSession connInfo sess = do
  conE <- connect connInfo
  case conE of
    Left e -> pure $ Left e
    Right con -> runExceptT $ runReaderT (getSession sess) con

query_ :: forall r.(FromRow r) => Query -> Session (Vector r)
query_ q = do
  con <- ask
  res <- liftIO $ query con q
  case res of
    Left es -> throwSQLErr es
    Right r -> pure r

execute_ :: Query -> Session Int64
execute_ q = do
  con <- ask
  res <- liftIO $ execute con q
  case res of
    Left es -> throwSQLErr es
    Right r -> pure r

data HandleRef
  = SQLENVRef (Ptr SQLHENV)
  | SQLDBCRef (Ptr SQLHDBC)
  | SQLSTMTRef (Ptr SQLHSTMT)
  
getMessages :: HandleRef -> IO (Either SQLError [(T.Text, T.Text)])
getMessages handleRef = do
  msgsRef <- newIORef []
  appendMessage <- appendMessageM msgsRef
  let
    (HandleType handleType, handle, handleName) = case handleRef of
      SQLENVRef h -> (SQL_HANDLE_ENV, castPtr h, "ENV")
      SQLDBCRef h -> (SQL_HANDLE_DBC, castPtr h, "DATABASE")
      SQLSTMTRef h -> (SQL_HANDLE_STMT, castPtr h, "STATEMENT")
  ret <- [C.block| int {
             SQLRETURN ret = 0;
             SQLSMALLINT i = 0;
             SQLWCHAR eState [6]; 
             SQLWCHAR eMSG [SQL_MAX_MESSAGE_LENGTH];
             SQLSMALLINT eMSGLen;
             SQLHANDLE handle = $(SQLHANDLE handle);
             void (*appendMessage)(SQLWCHAR*, int, SQLWCHAR*, int) = $(void (*appendMessage)(SQLWCHAR*, int, SQLWCHAR*, int));
             do {
               ret = SQLGetDiagRecW((SQLSMALLINT)$(int handleType), handle, ++i, eState, NULL, eMSG, 1000, &eMSGLen);
               if (SQL_SUCCEEDED(ret)) {
                  appendMessage(eState, 5, eMSG, eMSGLen);
               }
             } while( ret == SQL_SUCCESS );

             if (!SQL_SUCCEEDED(ret)) return ret;
             
             
             return 0;
         }|]
  case ret of
    0    -> Right <$> readIORef msgsRef
    100  -> Right <$> readIORef msgsRef 
    -2 -> pure $ Left $ SQLError
            { sqlState = ""
            , sqlMessage = "Invalid " <> handleName <> " handle"
            , sqlReturn  = -2
            }
    e  -> pure $ Left $ SQLError
            { sqlState = ""
            , sqlMessage = "UNKNOWN ERROR"
            , sqlReturn  = fromIntegral e
            }
  where
    appendMessageM :: IORef [(T.Text, T.Text)] -> IO (FunPtr (Ptr CWchar -> CInt -> Ptr CWchar -> CInt -> IO ()))
    appendMessageM msgsRef = $(C.mkFunPtr [t| Ptr CWchar
                                 -> C.CInt
                                 -> Ptr CWchar
                                 -> C.CInt
                                 -> IO ()
                                |]) $ \state _stateLen msg msgLen -> do
      msgText <- fromPtr (coerce msg) (fromIntegral msgLen)
      stateText <- fromPtr (coerce state) 5
      modifyIORef' msgsRef (\ms -> ms ++ [(msgText, stateText)])
      pure ()

getErrors :: ResIndicator -> HandleRef -> IO SQLErrors
getErrors res handleRef = do
  msgE <- getMessages handleRef
  pure $ SQLErrors $ case msgE of
    Left es -> [es]
    Right msgs -> fmap (\(msg, st) -> SQLError
                                      { sqlState = st
                                      , sqlMessage = msg
                                      , sqlReturn = coerce res
                                      }) msgs

sqldirect :: Connection -> Ptr SQLHSTMT -> T.Text -> IO (Either SQLErrors ())
sqldirect _con hstmt sql = do
  (queryWStr, queryLen) <- fmap (fmap fromIntegral) $ asForeignPtr $ sql
  numResultColsFP :: ForeignPtr CShort <- mallocForeignPtr

  ret <- ResIndicator <$> [C.block| int {
    SQLRETURN ret = 0;
    SQLHSTMT hstmt = $(SQLHSTMT hstmt);
    SQLSMALLINT* numColumnPtr = $fptr-ptr:(SQLSMALLINT* numResultColsFP);

    ret = SQLExecDirectW(hstmt, $fptr-ptr:(SQLWCHAR* queryWStr), $(int queryLen));
    if (!SQL_SUCCEEDED(ret)) return ret;

    ret = SQLNumResultCols(hstmt, numColumnPtr);

    return ret;
    }|]
  
  case isSuccessful ret of
    False -> Left <$> getErrors ret (SQLSTMTRef hstmt)      
    True -> pure $ Right ()
        
allocHSTMT :: Connection -> IO (Either SQLErrors (HSTMT a))
allocHSTMT con = do
  alloca $ \(hstmtp :: Ptr (Ptr SQLHSTMT)) -> do
    ret <- ResIndicator <$> [C.block| SQLRETURN {
      SQLRETURN ret = 0;
      SQLHSTMT* hstmtp = $(SQLHSTMT* hstmtp);
      SQLHDBC hdbc = $(SQLHDBC hdbc);
      ret = SQLAllocHandle(SQL_HANDLE_STMT, hdbc, hstmtp);
      return ret;
    }|]

    case isSuccessful ret of
      False -> Left <$> getErrors ret (SQLDBCRef hdbc)
      True -> do
        hstmt <- peek hstmtp
        cpos <- initColPos
        pure $ Right $ HSTMT hstmt cpos
  where
    hdbc = _hdbc con


releaseHSTMT :: HSTMT a -> IO (Either SQLErrors ())
releaseHSTMT stmt = do
  ret <- ResIndicator <$> [C.block| SQLRETURN {
      SQLRETURN ret = 0;
      SQLHSTMT hstmt =  $(SQLHSTMT hstmt);
      if (hstmt != SQL_NULL_HSTMT) {
        ret = SQLFreeHandle(SQL_HANDLE_STMT, hstmt);
      }
      return ret;
  }|]

  case isSuccessful ret of
    True -> pure $ Right ()
    False -> Left <$> getErrors ret (SQLSTMTRef hstmt)
  where
    hstmt = getHSTMT stmt

withHSTMT :: Connection -> (HSTMT a -> IO (Either SQLErrors a)) -> IO (Either SQLErrors a)
withHSTMT con act = do
  hstmtE <- allocHSTMT con
  case hstmtE of
    Right hstmt -> do
      resE <- act hstmt
      relE <- releaseHSTMT hstmt
      pure $ join $ fmap (const resE) relE
    Left e -> pure $ Left e
  

sqlFetch :: HSTMT a -> IO CInt
sqlFetch stmt = do
  [C.block| int {
      SQLRETURN ret = 0;
      SQLHSTMT hstmt = $(SQLHSTMT hstmt);

      ret = SQLFetch(hstmt);

      return ret;
  }|]
  where
    hstmt = getHSTMT stmt

type InfoType = Int
-- getInfo return either string or ulong based on infotype. TODO: needs to handle that
sqlGetInfo :: Connection -> InfoType -> IO (Either SQLErrors T.Text)
sqlGetInfo con 0  = do
  (infoFP :: ForeignPtr Word16) <- mallocForeignPtrBytes (16 * 1024)
  (bufferSizeOut :: ForeignPtr Int) <- mallocForeignPtr
  ret <- ResIndicator <$> [C.block| SQLRETURN {
      SQLRETURN ret = 0;
      SQLHDBC hdbc = $(SQLHDBC hdbc);
      ret = SQLGetInfo(hdbc, SQL_DATABASE_NAME, $fptr-ptr:(SQLWCHAR* infoFP), (SQLSMALLINT)(16 * 1024), $fptr-ptr:(SQLSMALLINT * bufferSizeOut));
      return ret;
  }|]

  case isSuccessful ret of
    False -> Left <$> getErrors ret (SQLDBCRef hdbc)
    True -> do
      bufferSize <- withForeignPtr bufferSizeOut peek
      info <- withForeignPtr infoFP $ \infoP -> fromPtr infoP (round ((fromIntegral bufferSize :: Double)/2))
      pure $ Right info
  where
    hdbc = _hdbc con
sqlGetInfo con _  = do
  (infoFP :: ForeignPtr CULong) <- mallocForeignPtr
  withForeignPtr infoFP $ \infoP -> do
    ret <- ResIndicator <$> [C.block| SQLRETURN {
             SQLRETURN ret = 0;
             SQLHDBC hdbc = $(SQLHDBC hdbc);
             ret = SQLGetInfo(hdbc, SQL_MAX_CONCURRENT_ACTIVITIES, $(SQLUINTEGER* infoP), (SQLSMALLINT)(sizeof(SQLUINTEGER)), NULL);
             return ret;
           }|]

    case isSuccessful ret of
      False -> Left <$> getErrors ret (SQLDBCRef hdbc)
      True -> do
        (Right . T.pack . show) <$> peek infoP
  where
    hdbc = _hdbc con

data ColDescriptor = ColDescriptor
  { colName         :: T.Text
  , colDataType     :: SQLType
  , colSize         :: Word
  , colDecimalDigit :: Int
  , colIsNullable   :: Maybe Bool
  , colPosition     :: CUShort
  } deriving (Show, Eq)

-- NOTE: use SQLGetTypeInfo to get signed info
sqlDescribeCol :: Ptr SQLHSTMT -> CUShort -> IO (Either SQLErrors ColDescriptor)
sqlDescribeCol hstmt colPos' = do
  (nameLengthFP :: ForeignPtr CShort) <- mallocForeignPtr
  (dataTypeFP :: ForeignPtr CShort) <- mallocForeignPtr
  (decimalDigitsFP :: ForeignPtr CShort) <- mallocForeignPtr
  (nullableFP :: ForeignPtr CShort) <- mallocForeignPtr
  (colSizeFP :: ForeignPtr CULong) <- mallocForeignPtr
  (tabNameFP :: ForeignPtr Word16) <- mallocForeignPtrBytes (16 * 128)
  withForeignPtr nameLengthFP $ \nameLengthP -> do
    withForeignPtr dataTypeFP $ \dataTypeP -> do
       withForeignPtr decimalDigitsFP $ \decimalDigitsP -> do
         withForeignPtr nullableFP $ \nullableP -> do
           withForeignPtr colSizeFP $ \colSizeP -> do
             withForeignPtr (castForeignPtr tabNameFP) $ \tabNameP -> do
               ret <- ResIndicator <$> [C.block| SQLRETURN {
                          SQLRETURN ret = 0;
                          SQLHSTMT hstmt = $(SQLHSTMT hstmt);
                          SQLSMALLINT* nameLengthP = $(SQLSMALLINT* nameLengthP);
                          SQLSMALLINT* dataTypeP = $(SQLSMALLINT* dataTypeP);
                          SQLSMALLINT* decimalDigitsP = $(SQLSMALLINT* decimalDigitsP);
                          SQLSMALLINT* nullableP = $(SQLSMALLINT* nullableP);
                          SQLULEN* colSizeP = $(SQLULEN* colSizeP);
                          SQLWCHAR* tabNameP = $(SQLWCHAR* tabNameP);
               
                          ret = SQLDescribeColW(hstmt, $(SQLUSMALLINT colPos'), tabNameP, 16 * 128, nameLengthP, dataTypeP, colSizeP, decimalDigitsP, nullableP);
                          return ret;
                      }|]

               case isSuccessful ret of
                 False -> Left <$> getErrors ret (SQLSTMTRef hstmt)
                 True -> do
                   nameLength <- peek nameLengthP
                   tableName <- fromPtr (castPtr tabNameP) (fromIntegral nameLength)
                   dataType <- peek dataTypeP
                   decimalDigits <- peek decimalDigitsP
                   cSize <- peek colSizeP
                   nullable <- peek nullableP
                   pure $ Right $ ColDescriptor
                    { colName = tableName
                    , colPosition = colPos'
                    , colDataType = SQLType dataType
                    , colSize = fromIntegral cSize
                    , colDecimalDigit = fromIntegral decimalDigits
                    , colIsNullable = case NullableFieldDesc nullable of
                        SQL_NO_NULLS         -> Just False
                        SQL_NULLABLE         -> Just True
                        SQL_NULLABLE_UNKNOWN -> Nothing
#if __GLASGOW_HASKELL__ < 820
                        _                    -> error "Panic: impossible case"
#endif
                    }
  
isSuccessful :: ResIndicator -> Bool
isSuccessful SQL_SUCCESS           = True
isSuccessful SQL_SUCCESS_WITH_INFO = True
isSuccessful _                     = False

extractVal :: Storable t => ColBufferType 'Bound t -> IO t
extractVal cbuff = case cbuff of
  BoundColBuffer _ cbuffPtr -> withForeignPtr cbuffPtr peek

extractWith ::
  Storable t =>
  ColBufferType 'Bound t ->
  (CLong -> Ptr t -> IO b) ->
  IO b
extractWith cbuff f = case cbuff of
  BoundColBuffer bufSizeFP cbuffPtr -> do
    -- NOTE: what about errors?
    bufSize <- peekFP bufSizeFP 
    withForeignPtr cbuffPtr (f bufSize)

castColBufferPtr ::
  ColBufferType 'Bound t1 ->
  ColBufferType 'Bound t2
castColBufferPtr cbuff = case cbuff of
  BoundColBuffer lenOrIndFP cbuffPtr ->
    BoundColBuffer lenOrIndFP (castForeignPtr cbuffPtr)

unboundWith :: 
  Storable t =>
  ColBufferType 'Unbound t ->
  a ->
  (CLong -> Ptr t -> a -> IO a) ->
  IO a
unboundWith cbuff a f =
  case cbuff of
    UnboundColBuffer k -> k a f

{-    
getLengthOrIndicator :: ColBuffer t -> IO (Either CLong ResIndicator)
getLengthOrIndicator cb = do
  lenOrInd <- fromIntegral <$> (peekFP $ lengthOrIndicatorFP cb)
  pure $ case lenOrInd `elem` [SQL_NULL_DATA, SQL_NO_TOTAL] of
    True -> Right lenOrInd
    False -> Left $ fromIntegral lenOrInd
-}

data ColBufferTypeK =
    Bound
  | Unbound
  
data ColBufferType (k :: ColBufferTypeK) t where
  BoundColBuffer   :: ForeignPtr CLong -> ForeignPtr t -> ColBufferType 'Bound t
  UnboundColBuffer :: (forall a. a -> (CLong -> Ptr t -> a -> IO a) -> IO a) -> ColBufferType 'Unbound t

boundColBuffer :: ForeignPtr CLong -> ForeignPtr t -> ColBufferType 'Bound t
boundColBuffer = BoundColBuffer

unboundColBuffer ::
  (forall a. a -> (CLong -> Ptr t -> a -> IO a) -> IO a) ->
  ColBufferType 'Unbound t
unboundColBuffer = UnboundColBuffer

type family GetColBufferType t where
  GetColBufferType (CUnbound _) = 'Unbound
  GetColBufferType _            = 'Bound

newtype CUnbound a = CUnbound { getCUnbound :: a }
                   deriving (Show, Eq, Ord, Enum, Bounded, Num, Integral, Real, Storable)

newtype CSized (size :: Nat) a = CSized { getCSized :: a }
                              deriving (Show, Eq, Ord, Enum, Bounded, Num, Integral, Real, Storable)

newtype ColBuffer t = ColBuffer
  { getColBuffer :: ColBufferType (GetColBufferType t) t
  }
  
fetchRows :: HSTMT a -> IO r -> IO (Vector r, ResIndicator)
fetchRows hstmt rowP = do
  retRef <- newIORef SQL_SUCCESS
  rows <- flip V.unfoldrM () $ \_ -> do
    res <- sqlFetch hstmt
    case ResIndicator $ fromIntegral res of
      SQL_SUCCESS -> do
        r <- rowP
        pure $ Just (r, ())
    
      ret -> atomicModifyIORef' retRef (\r -> (ret, r)) *> pure Nothing
  ret <- readIORef retRef
  pure (rows, ret)

sqlRowCount :: HSTMT a -> IO (Either SQLErrors Int64)
sqlRowCount stmt = do
  (rcountFP :: ForeignPtr CLong) <- mallocForeignPtr
  ret <- fmap ResIndicator $ withForeignPtr rcountFP $ \rcountP -> do
    [C.block| int {
        SQLRETURN ret = 0;
        SQLHSTMT hstmt = $(SQLHSTMT hstmt);

        ret = SQLRowCount(hstmt, $(SQLLEN* rcountP));

        return ret;
    }|]

  case isSuccessful ret of
    True -> (Right . fromIntegral) <$> peekFP rcountFP
    False -> Left <$> getErrors ret (SQLSTMTRef hstmt)
  where
    hstmt = getHSTMT stmt


data FieldDescriptor t = FieldDescriptor

initColPos :: IO ColPos
initColPos = ColPos <$> newIORef 1

getCurrentColDescriptor :: HSTMT a -> IO (Either SQLErrors ColDescriptor)
getCurrentColDescriptor hstmt = do
  let (ColPos wref) = colPos hstmt
  currColPos <- readIORef wref
  sqlDescribeCol (getHSTMT hstmt) ( currColPos)

getCurrentColDescriptorAndMove :: HSTMT a -> IO (Either SQLErrors ColDescriptor)
getCurrentColDescriptorAndMove hstmt = do
  let (ColPos wref) = colPos hstmt
  currColPos <- atomicModifyIORef' wref (\w -> (w +1, w))
  res <- sqlDescribeCol (getHSTMT hstmt) (fromIntegral currColPos)
  pure res

nextColPos :: HSTMT a -> IO ()
nextColPos hstmt = do
  let (ColPos wref) = colPos hstmt
  atomicModifyIORef' wref (\w -> (w +1, ()))

getColPos :: HSTMT a -> IO CUShort
getColPos hstmt = do
  let (ColPos wref) = colPos hstmt
  readIORef wref

type Query = T.Text

query :: forall r.(FromRow r) => Connection -> Query -> IO (Either SQLErrors (Vector r))
query = queryWith fromRow 

queryWith :: forall r.RowParser r -> Connection -> Query -> IO (Either SQLErrors (Vector r))
queryWith (RowParser colBuf rowPFun) con q = do
  withHSTMT con $ \hstmt -> do
    resE <- sqldirect con (getHSTMT hstmt) q
    case resE of
      Left es -> pure $ Left es
      Right _ -> do
        colBufferE <- colBuf ((coerce hstmt) :: HSTMT ())
        case colBufferE of
          Right colBuffer -> do
            (rows, ret) <- fetchRows hstmt (rowPFun colBuffer)
            case ret of
              SQL_SUCCESS           -> pure $ Right rows
              SQL_SUCCESS_WITH_INFO -> pure $ Right rows
              SQL_NO_DATA           -> pure $ Right rows
              _                     -> do
                errs <- getErrors ret (SQLSTMTRef $ getHSTMT hstmt)

                pure (Left errs)
          Left e -> do
            pure $ Left e

execute :: Connection -> Query -> IO (Either SQLErrors Int64)
execute con q = do
  withHSTMT con $ \hstmt -> do
    resE <- sqldirect con (getHSTMT hstmt) q
    join <$> traverse (const $ sqlRowCount hstmt) resE
  
data RowParser t =
  forall rowbuff.
  RowParser { rowBuffer    :: HSTMT () -> IO (Either SQLErrors rowbuff)
            , runRowParser :: rowbuff -> IO t
            }

instance Functor RowParser where
  fmap f (RowParser b rpf) = RowParser b $ \b' -> do
    res <- rpf b'
    pure (f res)

instance Applicative RowParser where
  pure a = RowParser (pure . pure . const ()) (const (pure a))
  (RowParser b1 f) <*> (RowParser b2 a) =
    RowParser (\hstmt -> (,) <$$> b1 hstmt <**> b2 hstmt) $
    \(b1', b2') -> f b1' <*> a b2'

field :: forall f. FromField f => RowParser f
field = RowParser
        (\s ->
            sqlBindCol (restmt s :: HSTMT (ColBuffer (FieldBufferType f)))
        )
        fromField

restmt :: forall a b. HSTMT a -> HSTMT b
restmt (HSTMT stm cp) = HSTMT stm cp :: HSTMT b

class FromRow t where
  fromRow :: RowParser t

  default fromRow :: ( Generic t
                     , GFromRow (Rep t)
                     ) => RowParser t
  fromRow = to <$> gFromRow

class GFromRow (f :: * -> *) where
  gFromRow :: RowParser (f a)

instance (GFromRow f) => GFromRow (M1 c i f) where
  gFromRow = M1 <$> gFromRow 

instance GFromRow U1 where
  gFromRow = pure U1

instance (GFromRow f, GFromRow g) => GFromRow (f :*: g) where
  gFromRow = 
    (:*:) <$> gFromRow
          <*> gFromRow

instance (FromField a) => GFromRow (K1 k a) where
  gFromRow = K1 <$> field
    
instance (FromField a, FromField b) => FromRow (a, b) where
  fromRow =  (,) <$> field <*> field

instance FromField a => FromRow (Identity a) where
  fromRow = Identity <$> field

type FieldParser t = ColBuffer (FieldBufferType t) -> IO t

class ( SQLBindCol ((ColBuffer (FieldBufferType t)))
      ) => FromField t where
  type FieldBufferType t :: *
  fromField :: ColBuffer (FieldBufferType t) -> IO t

instance FromField Int where
  type FieldBufferType Int = CBigInt
  fromField = \i -> extractVal (getColBuffer i) >>= (\v -> pure $ fromIntegral v)
  
{-
instance FromField Int8 where
  type FieldBufferType Int8 = CTinyInt
  fromField = Value $ \i -> extractVal i >>= (\v -> pure $ fromIntegral v)
-}
  
instance FromField Int16 where
  type FieldBufferType Int16 = CSmallInt 
  fromField = \i -> extractVal (getColBuffer i) >>= (\v -> pure $ fromIntegral v)

instance FromField Int32 where
  type FieldBufferType Int32 = CLong
  fromField = \i -> extractVal (getColBuffer i) >>= (\v -> pure $ fromIntegral v)

instance FromField Int64 where
  type FieldBufferType Int64 = CBigInt
  fromField = \i -> extractVal (getColBuffer i) >>= (\v -> pure $ coerce v)

{-
instance FromField Word where
  type FieldBufferType Word = CUBigInt
  fromField = Value $ \i -> extractVal i >>= (\v -> pure $ fromIntegral v)
-}

instance FromField Word8 where
  type FieldBufferType Word8 = CUTinyInt
  fromField = \i -> extractVal (getColBuffer i) >>= (\v -> pure $ fromIntegral v)

{-
instance FromField Word16 where
  type FieldBufferType Word16 = CUSmallInt
  fromField = Value $ \i -> extractVal i >>= (\v -> pure $ fromIntegral v)

instance FromField Word32 where
  type FieldBufferType Word32 = CULong
  fromField = Value $ \i -> extractVal i >>= (\v -> pure $ fromIntegral v)

instance FromField Word64 where
  type FieldBufferType Word64 = CUBigInt
  fromField = Value $ \i -> extractVal i >>= (\v -> pure $ fromIntegral v)  
-}

instance FromField Double where
  type FieldBufferType Double = CDouble
  fromField = \i -> extractVal (getColBuffer i) >>= (\v -> pure $ coerce v)

instance FromField Float where
  type FieldBufferType Float = CFloat
  fromField = \i -> extractVal (getColBuffer i) >>= (\v -> pure $ coerce v)

instance FromField Bool where
  type FieldBufferType Bool = CBool
  fromField = \i -> extractVal (getColBuffer i) >>= (\v -> pure $ if v == 1 then True else False)

newtype ASCIIText = ASCIIText { getAsciiText :: T.Text }
                  deriving (Show, Eq)

instance FromField ASCIIText where
  type FieldBufferType ASCIIText = CUnbound CChar
  fromField = \v -> do
    bsb <- unboundWith (getColBuffer v) mempty $
      \bufSize ccharP acc -> do
        a <- BS.packCStringLen (coerce ccharP, fromIntegral bufSize)
        pure (acc <> BSB.byteString a)
    pure (ASCIIText . T.pack . BS8.unpack . LBS.toStrict $ BSB.toLazyByteString bsb)  

instance FromField ByteString where
  type FieldBufferType ByteString = CUnbound CBinary
  fromField = \v -> do
    bsb <- unboundWith (getColBuffer v) mempty $
      \bufSize ccharP acc -> do
        a <- BS.packCStringLen (coerce ccharP, fromIntegral bufSize)
        pure (acc <> BSB.byteString a)
    pure (LBS.toStrict $ BSB.toLazyByteString bsb)

instance FromField Image where
  type FieldBufferType Image = CUnbound CBinary
  fromField = \v -> do
    bsb <- unboundWith (getColBuffer v) mempty $
      \bufSize ccharP acc -> do
        a <- BS.packCStringLen (coerce ccharP, fromIntegral bufSize)
        pure (acc <> BSB.byteString a)
    pure (coerce $ BSB.toLazyByteString bsb)

instance FromField T.Text where
  type FieldBufferType T.Text = CUnbound CWchar
  fromField = \v -> do
    lzt <- unboundWith (getColBuffer v) mempty $
      \bufSize cwcharP acc -> do
        let clen = bufSize - 1 --  round ((fromIntegral bufSize :: Double)/2) :: Word
        a <- T.fromPtr (coerce cwcharP) (fromIntegral clen)
        pure (acc <> LTB.fromText a)
    pure (LT.toStrict (LTB.toLazyText lzt))

instance FromField Money where
  type FieldBufferType Money = CMoney
  fromField = \v -> do
    extractWith (castColBufferPtr $ getColBuffer v) $ \bufSize ccharP -> do
      -- let clen = round ((fromIntegral bufSize :: Double)/2) :: Word
      a <- BS.packCStringLen (ccharP, fromIntegral bufSize)      
      -- rawT <- T.fromPtr ccharP (fromIntegral clen)
      print $ "X: Money: " ++ show a
      let res = readMaybe . BS8.unpack $ a -- rawT
      maybe (error $ "Parse failed for Money: " ++ show a {-rawT-})
            (pure . Money) res

instance FromField SmallMoney where
  type FieldBufferType SmallMoney = CMoney
  fromField = \v -> do
    extractWith (castColBufferPtr $ getColBuffer v) $ \bufSize cwcharP -> do
      let clen = round ((fromIntegral bufSize :: Double)/2) :: Word      
      rawT <- T.fromPtr cwcharP (fromIntegral clen)
      let res = readMaybe . T.unpack $ rawT
      maybe (error $ "Parse failed for SmallMoney: " ++ show rawT)
            (pure . SmallMoney) res

instance FromField Day where
  type FieldBufferType Day = CDate
  fromField = \i -> extractVal (getColBuffer i) >>= (\v -> pure $ getDate v)

instance FromField TimeOfDay where
  type FieldBufferType TimeOfDay = CTimeOfDay
  fromField = \i -> extractVal (getColBuffer i) >>= (\v -> pure $ getTimeOfDay v)

instance FromField LocalTime where
  type FieldBufferType LocalTime = CLocalTime
  fromField = \i -> extractVal (getColBuffer i) >>= (\v -> pure $ getLocalTime v)

instance FromField ZonedTime where
  type FieldBufferType ZonedTime = CZonedTime
  fromField = \i -> extractVal (getColBuffer i) >>= (\v -> pure $ Database.MsSQL.Internal.SQLTypes.getZonedTime v)

instance FromField UTCTime where
  type FieldBufferType UTCTime = FieldBufferType ZonedTime
  fromField = \v -> zonedTimeToUTC <$> fromField v

instance FromField UUID where
  type FieldBufferType UUID = UUID
  fromField = \i -> extractVal (getColBuffer i)

-- NOTE: There is no generic lengthOrIndicatorFP
instance FromField a => FromField (Maybe a) where
  type FieldBufferType (Maybe a) = FieldBufferType a
  fromField = \v -> do
    case getColBuffer v of
      BoundColBuffer fptr _ -> do 
        lengthOrIndicator <- peekFP fptr
        if lengthOrIndicator == fromIntegral SQL_NULL_DATA -- TODO: Only long worked not SQLINTEGER
          then pure Nothing
          else Just <$> (fromField v)
      UnboundColBuffer _ -> pure Nothing
  
instance FromField a => FromField (Identity a) where
  type FieldBufferType (Identity a) = FieldBufferType a
  fromField = \v ->
      Identity <$> (fromField v)

-- TODO: Is coerceColBuffer necessary? is it same as castColBufferPtr?
-- coerceColBuffer :: (Coercible a b) => ColBuffer a -> ColBuffer b
-- coerceColBuffer c = c {getColBuffer  = castForeignPtr $ getColBuffer c}

peekFP :: Storable t => ForeignPtr t -> IO t
peekFP fp = withForeignPtr fp peek

type SQLBindColM t = ReaderT ColPos IO (Either SQLErrors t)

class SQLBindCol t where
  sqlBindCol :: HSTMT t -> IO (Either SQLErrors t)

sqlBindColTpl :: forall t. (Typeable t) =>
                      HSTMT (ColBuffer t) ->
                      (Ptr SQLHSTMT -> ColDescriptor -> IO (Either SQLErrors (ColBuffer t))) ->
                      IO (Either SQLErrors (ColBuffer t))
sqlBindColTpl hstmt block = do
   let hstmtP = getHSTMT hstmt       
   colDescE <- getCurrentColDescriptorAndMove hstmt
   case colDescE of
     Left e -> pure $ Left e
     Right cdesc
       | match cdesc -> block hstmtP cdesc
       | otherwise   -> typeMismatch exps cdesc

     where match cdesc = colDataType cdesc `elem` exps
           exps        = maybe [] id (HM.lookup rep sqlMapping)
           rep         = typeOf (undefined :: t)

sqlBindColTplUnbound :: forall t. (Typeable t) =>
                      HSTMT (ColBuffer (CUnbound t)) ->
                      (Ptr SQLHSTMT -> ColDescriptor -> IO (Either SQLErrors (ColBuffer (CUnbound t)))) ->
                      IO (Either SQLErrors (ColBuffer (CUnbound t)))
sqlBindColTplUnbound hstmt block = do
   let hstmtP = getHSTMT hstmt       
   colDescE <- getCurrentColDescriptorAndMove hstmt
   case colDescE of
     Left e -> pure $ Left e
     Right cdesc
       | match cdesc -> block hstmtP cdesc
       | otherwise   -> typeMismatch exps cdesc

     where match cdesc = colDataType cdesc `elem` exps
           exps        = maybe [] id (HM.lookup rep sqlMapping)
           rep         = typeOf (undefined :: t)

newtype CBinary = CBinary { getCBinary :: CChar }
                deriving (Show, Eq, Ord, Enum, Bounded, Num, Integral, Real, Storable)


instance SQLBindCol (ColBuffer (CUnbound CBinary)) where
  sqlBindCol hstmt = 
   sqlBindColTplUnbound hstmt block
   
     where block hstmtP cdesc = do
             let bufSize = fromIntegral (20 :: Int)
             binFP <- mallocForeignPtrBytes (fromIntegral bufSize + 1)
             lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
             let cpos = colPosition cdesc
                 cbuf = unboundColBuffer (\acc f ->
                                               fetchBytes hstmtP binFP lenOrIndFP bufSize cpos f acc)
                        
             pure $ pure $ (ColBuffer cbuf)

           fetchBytes hstmtP binFP lenOrIndFP bufSize cpos f = go
           
             where go acc = do
                     ret <- fmap ResIndicator $ withForeignPtr binFP $ \binP -> do
                      [C.block| int {
                        SQLRETURN ret = 0;
                        SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
                        SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
                        ret = SQLGetData(hstmt, $(SQLUSMALLINT cpos), SQL_C_BINARY, $(SQLCHAR* binP), $(SQLLEN bufSize), lenOrInd);
                        return ret;
                      }|]
                     case isSuccessful ret of
                       True -> do
                           lengthOrInd <- peekFP lenOrIndFP
                           let actBufSize = case fromIntegral lengthOrInd of
                                              SQL_NO_TOTAL                 -> bufSize
                                              i | i >= fromIntegral bufSize -> bufSize
                                              _                            -> lengthOrInd                                              
                           acc' <- withForeignPtr binFP $ \tptr -> f actBufSize (coerce tptr) acc
                           go acc'
                       False -> pure acc

instance SQLBindCol (ColBuffer (CUnbound CChar)) where
  sqlBindCol hstmt = 
   sqlBindColTplUnbound hstmt block
   
     where block hstmtP cdesc = do
             let bufSize = fromIntegral (20 :: Int)
             chrFP <- mallocForeignPtrBytes (fromIntegral bufSize + 1)
             lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
             let cpos = colPosition cdesc
                 cbuf = unboundColBuffer (\acc f ->
                                               fetchBytes hstmtP chrFP lenOrIndFP bufSize cpos f acc)
                        
             pure $ pure $ (ColBuffer cbuf)

           fetchBytes hstmtP chrFP lenOrIndFP bufSize cpos f = go
           
             where go acc = do
                     ret <- fmap ResIndicator $ withForeignPtr chrFP $ \chrP -> do
                      [C.block| int {
                        SQLRETURN ret = 0;
                        SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
                        SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
                        ret = SQLGetData(hstmt, $(SQLUSMALLINT cpos), SQL_C_CHAR, $(SQLCHAR* chrP), $(SQLLEN bufSize), lenOrInd);
                        return ret;
                      }|]
                     case isSuccessful ret of
                       True -> do
                           lengthOrInd <- peekFP lenOrIndFP
                           let actBufSize = case fromIntegral lengthOrInd of
                                              SQL_NO_TOTAL                 -> bufSize - 1
                                              i | i > fromIntegral bufSize -> bufSize - 1
                                              _                            -> lengthOrInd
                           acc' <- withForeignPtr chrFP $ \tptr -> f actBufSize (coerce tptr) acc
                           go acc'
                       False -> pure acc


-- TODO: Test when buffersize is less than col size, read is not failing and simply empty row are returning
instance SQLBindCol (ColBuffer CChar) where
  sqlBindCol hstmt =
    sqlBindColTpl hstmt $ \hstmtP cdesc -> do
      let
        cpos = colPosition cdesc
        bufSize = fromIntegral $ colSize cdesc + 1
      chrFP <- mallocForeignPtrBytes (fromIntegral bufSize)
      lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
      ret <- fmap ResIndicator $ withForeignPtr chrFP $ \chrp -> do
        [C.block| int {
          SQLRETURN ret = 0;
          SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
          SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
          ret = SQLBindCol(hstmt, $(SQLUSMALLINT cpos), SQL_C_CHAR, $(SQLCHAR* chrp), $(SQLLEN bufSize), lenOrInd);
          return ret;
        }|]

      case isSuccessful ret of
        True -> do
          pure $ Right (ColBuffer (boundColBuffer lenOrIndFP chrFP) )
        False -> Left <$> getErrors ret (SQLSTMTRef hstmtP)

instance SQLBindCol (ColBuffer CBinary) where
  sqlBindCol hstmt =
    sqlBindColTpl hstmt $ \hstmtP cdesc -> do
      let
        cpos = colPosition cdesc
        bufSize = fromIntegral $ colSize cdesc + 1
      binFP <- mallocForeignPtrBytes (fromIntegral bufSize)
      lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
      ret <- fmap ResIndicator $ withForeignPtr binFP $ \binP -> do
        [C.block| int {
          SQLRETURN ret = 0;
          SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
          SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
          ret = SQLBindCol(hstmt, $(SQLUSMALLINT cpos), SQL_C_BINARY, $(SQLCHAR* binP), $(SQLLEN bufSize), lenOrInd);
          return ret;
        }|]

      case isSuccessful ret of
        True -> do
          pure $ Right (ColBuffer (boundColBuffer lenOrIndFP (coerce binFP)))
        False -> Left <$> getErrors ret (SQLSTMTRef hstmtP)

instance SQLBindCol (ColBuffer CWchar) where
  sqlBindCol hstmt =
    sqlBindColTpl hstmt $ \hstmtP cdesc -> do
      let cpos = colPosition cdesc
          bufSize = fromIntegral $ colSize cdesc + 1
      txtFP <- mallocForeignPtrBytes (fromIntegral bufSize)
      lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
      ret <- fmap ResIndicator $ withForeignPtr txtFP $ \txtP -> do
        [C.block| int {
          SQLRETURN ret = 0;
          SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
          SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
          ret = SQLBindCol(hstmt, $(SQLUSMALLINT cpos), SQL_C_WCHAR, $(SQLWCHAR* txtP), $(SQLLEN bufSize), lenOrInd);
          return ret;
        }|]

      case isSuccessful ret of
        True -> do
          pure $ Right (ColBuffer (boundColBuffer lenOrIndFP $ coerce txtFP))
        False -> Left <$> getErrors ret (SQLSTMTRef hstmtP)

instance SQLBindCol (ColBuffer (CUnbound CWchar)) where
  sqlBindCol hstmt = 
   sqlBindColTplUnbound hstmt block
   
     where block hstmtP cdesc = do
             let bufSize = fromIntegral (20 :: Int)
             txtFP <- mallocForeignPtrBytes (fromIntegral bufSize + 1)
             lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
             let cpos = colPosition cdesc
                 cbuf = unboundColBuffer (\acc f ->
                                               fetchText hstmtP txtFP lenOrIndFP bufSize cpos f acc)
                        
             pure $ pure $ (ColBuffer cbuf)

           fetchText hstmtP txtFP lenOrIndFP bufSize cpos f = go
           
             where go acc = do
                     ret <- fmap ResIndicator $ withForeignPtr txtFP $ \txtP -> do
                      [C.block| int {
                        SQLRETURN ret = 0;
                        SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
                        SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
                        ret = SQLGetData(hstmt, $(SQLUSMALLINT cpos), SQL_C_WCHAR, $(SQLWCHAR* txtP), $(SQLLEN bufSize), lenOrInd);
                        return ret;
                      }|]
                     case isSuccessful ret of
                       True -> do
                           lengthOrInd <- peekFP lenOrIndFP
                           let actBufSize = case fromIntegral lengthOrInd of
                                              SQL_NO_TOTAL -> bufSize - 2
                                              _            -> lengthOrInd                                              
                           acc' <- withForeignPtr txtFP $ \tptr -> f actBufSize (coerce tptr) acc
                           go acc'
                       False -> pure acc

-- NOTE: CMoney should be CDecimal
newtype CMoney = CMoney CChar
  deriving (Show, Eq, Ord, Enum, Bounded, Num, Integral, Real, Storable)

instance SQLBindCol (ColBuffer CMoney) where
  sqlBindCol hstmt =
    sqlBindColTpl hstmt $ \hstmtP cdesc -> do
      let cpos = colPosition cdesc
          bufSize = fromIntegral (colSize cdesc)
      monFP <- mallocForeignPtrBytes (fromIntegral bufSize + 1)
      lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
      ret <- fmap ResIndicator $ withForeignPtr monFP $ \monP -> do
        [C.block| int {
          SQLRETURN ret = 0;
          SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
          SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
          ret = SQLBindCol(hstmt, $(SQLUSMALLINT cpos), SQL_C_CHAR, $(SQLCHAR* monP), $(SQLLEN bufSize), lenOrInd);
          return ret;
        }|]

      case isSuccessful ret of
        True -> do
          pure $ Right (ColBuffer (boundColBuffer lenOrIndFP $ coerce monFP))
        False -> Left <$> getErrors ret (SQLSTMTRef hstmtP)

newtype CUTinyInt = CUTinyInt CUChar
                  deriving (Show, Eq, Ord, Enum, Bounded, Num, Integral, Real, Storable)

instance SQLBindCol (ColBuffer CUTinyInt) where
  sqlBindCol hstmt =
   sqlBindColTpl hstmt $ \hstmtP cdesc -> do
        chrFP <- mallocForeignPtr
        let cpos = colPosition cdesc
        lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
        ret <- fmap ResIndicator $ withForeignPtr chrFP $ \chrP -> do
         [C.block| int {
             SQLRETURN ret = 0;
             SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
             SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
             ret = SQLBindCol(hstmt, $(SQLUSMALLINT cpos), SQL_C_UTINYINT, $(SQLCHAR* chrP), sizeof(SQLCHAR), lenOrInd);
             return ret;
         }|]

        case isSuccessful ret of
          True -> do
            pure $ Right (ColBuffer (boundColBuffer lenOrIndFP $ castForeignPtr chrFP))
          False -> Left <$> getErrors ret (SQLSTMTRef hstmtP)


newtype CTinyInt = CTinyInt CChar
                  deriving (Show, Eq, Ord, Enum, Bounded, Num, Integral, Real, Storable)

instance SQLBindCol (ColBuffer CTinyInt) where
  sqlBindCol hstmt =
   sqlBindColTpl hstmt $ \hstmtP cdesc -> do
      chrFP <- mallocForeignPtr
      let cpos = colPosition cdesc
      lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
      ret <- fmap ResIndicator $ withForeignPtr chrFP $ \chrP -> do
        [C.block| int {
            SQLRETURN ret = 0;
            SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
            SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
            ret = SQLBindCol(hstmt, $(SQLUSMALLINT cpos), SQL_C_TINYINT, $(SQLCHAR* chrP), sizeof(SQLCHAR), lenOrInd);
            return ret;
        }|]

      case isSuccessful ret of
        True -> do
          pure $ Right (ColBuffer (boundColBuffer lenOrIndFP $ castForeignPtr chrFP))
        False -> Left <$> getErrors ret (SQLSTMTRef hstmtP)

instance SQLBindCol (ColBuffer CLong) where
  sqlBindCol hstmt = 
   sqlBindColTpl hstmt $ \hstmtP cdesc -> do
      let cpos = colPosition cdesc
      intFP <- mallocForeignPtr
      lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
      ret <- fmap ResIndicator $ withForeignPtr intFP $ \intP -> do
       [C.block| int {
           SQLRETURN ret = 0;
           SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
           SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
           ret = SQLBindCol(hstmt, $(SQLUSMALLINT cpos), SQL_C_LONG, $(SQLINTEGER* intP), sizeof(SQLINTEGER), lenOrInd);
           return ret;
       }|]

      case isSuccessful ret of
        True -> do
          pure $ Right (ColBuffer (boundColBuffer lenOrIndFP intFP))
        False -> Left <$> getErrors ret (SQLSTMTRef hstmtP)

instance SQLBindCol (ColBuffer CULong) where
  sqlBindCol hstmt =
   sqlBindColTpl hstmt $ \hstmtP cdesc -> do
      let cpos = colPosition cdesc
      intFP <- mallocForeignPtr
      lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
      ret <- fmap ResIndicator $ withForeignPtr intFP $ \intP -> do
       [C.block| int {
           SQLRETURN ret = 0;
           SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
           SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
           ret = SQLBindCol(hstmt, $(SQLUSMALLINT cpos), SQL_C_ULONG, $(SQLUINTEGER* intP), sizeof(SQLUINTEGER), lenOrInd);
           return ret;
       }|]

      case isSuccessful ret of
        True -> do
          pure $ Right (ColBuffer (boundColBuffer lenOrIndFP intFP))
        False -> Left <$> getErrors ret (SQLSTMTRef hstmtP)

newtype CSmallInt = CSmallInt CShort
                  deriving (Show, Eq, Ord, Enum, Bounded, Num, Integral, Real, Storable)

instance SQLBindCol (ColBuffer CSmallInt) where
  sqlBindCol hstmt = 
   sqlBindColTpl hstmt $ \hstmtP cdesc -> do 
       let cpos = colPosition cdesc
       shortFP <- mallocForeignPtr           
       lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
       ret <- fmap ResIndicator $ withForeignPtr shortFP $ \shortP -> do
        [C.block| int {
            SQLRETURN ret = 0;
            SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
            SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
            ret = SQLBindCol(hstmt, $(SQLUSMALLINT cpos), SQL_C_SHORT, $(SQLSMALLINT* shortP), sizeof(SQLSMALLINT), lenOrInd);
            return ret;
        }|]

       case isSuccessful ret of
         True -> do
           pure $ Right (ColBuffer (boundColBuffer lenOrIndFP $ coerce shortFP))
         False -> Left <$> getErrors ret (SQLSTMTRef hstmtP)

newtype CUSmallInt = CUSmallInt CShort
                  deriving (Show, Eq, Ord, Enum, Bounded, Num, Integral, Real, Storable)

instance SQLBindCol (ColBuffer CUSmallInt) where
  sqlBindCol hstmt =
   sqlBindColTpl hstmt $ \hstmtP cdesc -> do
      let cpos = colPosition cdesc
      shortFP <- mallocForeignPtr
      lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
      ret <- fmap ResIndicator $ withForeignPtr shortFP $ \shortP -> do
       [C.block| int {
           SQLRETURN ret = 0;
           SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
           SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
           ret = SQLBindCol(hstmt, $(SQLUSMALLINT cpos), SQL_C_USHORT, $(SQLUSMALLINT* shortP), sizeof(SQLUSMALLINT), lenOrInd);
           return ret;
       }|]

      case isSuccessful ret of
        True -> do
          pure $ Right (ColBuffer (boundColBuffer lenOrIndFP $ coerce shortFP))
        False -> Left <$> getErrors ret (SQLSTMTRef hstmtP)

instance SQLBindCol (ColBuffer CFloat) where
  sqlBindCol hstmt =
   sqlBindColTpl hstmt $ \hstmtP cdesc -> do
      let cpos = colPosition cdesc
      floatFP <- mallocForeignPtr
      lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
      ret <- fmap ResIndicator $ withForeignPtr floatFP $ \floatP -> do
       [C.block| int {
           SQLRETURN ret = 0;
           SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
           SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
           ret = SQLBindCol(hstmt, $(SQLUSMALLINT cpos), SQL_C_FLOAT, $(SQLREAL* floatP), sizeof(SQLREAL), lenOrInd);
           return ret;
       }|]

      case isSuccessful ret of
        True -> do
          pure $ Right (ColBuffer (boundColBuffer lenOrIndFP floatFP))
        False -> Left <$> getErrors ret (SQLSTMTRef hstmtP)

instance SQLBindCol (ColBuffer CDouble) where
  sqlBindCol hstmt =
   sqlBindColTpl hstmt $ \hstmtP cdesc -> do    
      let cpos = colPosition cdesc
      floatFP <- mallocForeignPtr
      lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
      ret <- fmap ResIndicator $ withForeignPtr floatFP $ \floatP -> do
       [C.block| int {
           SQLRETURN ret = 0;
           SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
           SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
           ret = SQLBindCol(hstmt, $(SQLUSMALLINT cpos), SQL_C_DOUBLE, $(SQLDOUBLE* floatP), sizeof(SQLDOUBLE), lenOrInd);
           return ret;
       }|]

      case isSuccessful ret of
        True -> do
          pure $ Right (ColBuffer (boundColBuffer lenOrIndFP floatFP))
        False -> Left <$> getErrors ret (SQLSTMTRef hstmtP)

instance SQLBindCol (ColBuffer CBool) where
  sqlBindCol hstmt =
   sqlBindColTpl hstmt $ \hstmtP cdesc -> do        
      let cpos = colPosition cdesc
      chrFP <- mallocForeignPtr
      lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
      ret <- fmap ResIndicator $ withForeignPtr chrFP $ \chrP -> do
       [C.block| int {
           SQLRETURN ret = 0;
           SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
           SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
           ret = SQLBindCol(hstmt, $(SQLUSMALLINT cpos), SQL_C_BIT, $(SQLCHAR* chrP), sizeof(1), lenOrInd);
           return ret;
       }|]

      case isSuccessful ret of
        True -> do
          pure $ Right (ColBuffer (boundColBuffer lenOrIndFP $ castForeignPtr chrFP))
        False -> Left <$> getErrors ret (SQLSTMTRef hstmtP)

instance SQLBindCol (ColBuffer CDate) where
  sqlBindCol hstmt = do
   sqlBindColTpl hstmt $ \hstmtP cdesc -> do
      let cpos = colPosition cdesc
      dateFP <- mallocForeignPtr
      lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
      ret <- fmap ResIndicator $ withForeignPtr dateFP $ \dateP -> do
       [C.block| int {
           SQLRETURN ret = 0;
           SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
           SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
           ret = SQLBindCol(hstmt, $(SQLUSMALLINT cpos), SQL_C_TYPE_DATE, $(SQL_DATE_STRUCT* dateP), sizeof(SQL_DATE_STRUCT), lenOrInd);
           return ret;
       }|]


      case isSuccessful ret of
        True -> do
          pure $ Right (ColBuffer (boundColBuffer lenOrIndFP dateFP))
        False -> Left <$> getErrors ret (SQLSTMTRef hstmtP)

newtype CBigInt = CBigInt CLLong
                  deriving (Show, Eq, Ord, Enum, Bounded, Num, Integral, Real, Storable)

instance SQLBindCol (ColBuffer CBigInt) where
  sqlBindCol hstmt =
   sqlBindColTpl hstmt $ \hstmtP cdesc -> do    
      let cpos = colPosition cdesc
      llongFP <- mallocForeignPtr
      lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
      ret <- fmap ResIndicator $ withForeignPtr llongFP $ \llongP -> do
       [C.block| int {
           SQLRETURN ret = 0;
           SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
           SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
           ret = SQLBindCol(hstmt, $(SQLUSMALLINT cpos), SQL_C_SBIGINT, $(long long* llongP), sizeof(long long), lenOrInd);
           return ret;
       }|]

      case isSuccessful ret of
        True -> do
          pure $ Right (ColBuffer (boundColBuffer lenOrIndFP $ coerce llongFP))
        False -> Left <$> getErrors ret (SQLSTMTRef hstmtP)

newtype CUBigInt = CUBigInt CULLong
                  deriving (Show, Eq, Ord, Enum, Bounded, Num, Integral, Real, Storable)

instance SQLBindCol (ColBuffer CUBigInt) where
  sqlBindCol hstmt =
   sqlBindColTpl hstmt $ \hstmtP cdesc -> do    
     let cpos = colPosition cdesc
     llongFP <- mallocForeignPtr           
     lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
     ret <- fmap ResIndicator $ withForeignPtr llongFP $ \llongP -> do
      [C.block| int {
          SQLRETURN ret = 0;
          SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
          SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
          ret = SQLBindCol(hstmt, $(SQLUSMALLINT cpos), SQL_C_UBIGINT, $(unsigned long long* llongP), sizeof(unsigned long long), lenOrInd);
          return ret;
      }|]

     case isSuccessful ret of
       True -> do
         pure $ Right (ColBuffer (boundColBuffer lenOrIndFP $ coerce llongFP))
       False -> Left <$> getErrors ret (SQLSTMTRef hstmtP)


instance SQLBindCol (ColBuffer CTimeOfDay) where
  sqlBindCol hstmt = do
   sqlBindColTpl hstmt $ \hstmtP cdesc -> do    
     let cpos = colPosition cdesc
     todFP <- mallocForeignPtr
     lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
     ret <- fmap ResIndicator $ withForeignPtr todFP $ \todP -> do
      [C.block| int {
          SQLRETURN ret = 0;
          SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
          SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
          ret = SQLBindCol(hstmt, $(SQLUSMALLINT cpos), SQL_C_BINARY, $(SQL_SS_TIME2_STRUCT* todP), sizeof(SQL_SS_TIME2_STRUCT), lenOrInd);
          return ret;
      }|]

     case isSuccessful ret of
       True -> do
         pure $ Right (ColBuffer (boundColBuffer lenOrIndFP todFP))
       False -> Left <$> getErrors ret (SQLSTMTRef hstmtP)
        
instance SQLBindCol (ColBuffer CLocalTime) where
  sqlBindCol hstmt = do
   sqlBindColTpl hstmt $
     \hstmtP cdesc -> do
       let cpos = colPosition cdesc
       ltimeFP <- mallocForeignPtr
       lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
       ret <- fmap ResIndicator $ withForeignPtr ltimeFP $ \ltimeP -> do
        [C.block| int {
            SQLRETURN ret = 0;
            SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
            SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
            ret = SQLBindCol(hstmt, $(SQLUSMALLINT cpos), SQL_C_TYPE_TIMESTAMP, $(SQL_TIMESTAMP_STRUCT* ltimeP), sizeof(SQL_TIMESTAMP_STRUCT), lenOrInd);
            return ret;
        }|]

       case isSuccessful ret of
         True -> do
           pure $ Right (ColBuffer (boundColBuffer lenOrIndFP ltimeFP))
         False -> Left <$> getErrors ret (SQLSTMTRef hstmtP)

instance SQLBindCol (ColBuffer CZonedTime) where
  sqlBindCol hstmt =
   sqlBindColTpl hstmt $ \hstmtP cdesc -> do
     let cpos = colPosition cdesc
     ltimeFP <- mallocForeignPtr
     lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
     ret <- fmap ResIndicator $ withForeignPtr ltimeFP $ \ltimeP -> do
      [C.block| int {
          SQLRETURN ret = 0;
          SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
          SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
          ret = SQLBindCol(hstmt, $(SQLUSMALLINT cpos), SQL_C_BINARY, $(SQL_SS_TIMESTAMPOFFSET_STRUCT* ltimeP), sizeof(SQL_SS_TIMESTAMPOFFSET_STRUCT), lenOrInd);
          return ret;
      }|]

     case isSuccessful ret of
       True -> do
         pure $ Right (ColBuffer (boundColBuffer lenOrIndFP ltimeFP))
       False -> Left <$> getErrors ret (SQLSTMTRef hstmtP)

instance SQLBindCol (ColBuffer UUID) where
  sqlBindCol hstmt =
   sqlBindColTpl hstmt $ \hstmtP cdesc -> do
     let cpos = colPosition cdesc
     uuidFP <- mallocForeignPtr
     lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
     ret <- fmap ResIndicator $ withForeignPtr uuidFP $ \uuidP -> do
      [C.block| int {
          SQLRETURN ret = 0;
          SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
          SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
          ret = SQLBindCol(hstmt, $(SQLUSMALLINT cpos), SQL_C_GUID, $(SQLGUID* uuidP), 16, lenOrInd);
          return ret;
      }|]

     case isSuccessful ret of
       True -> do
         pure $ Right (ColBuffer (boundColBuffer lenOrIndFP uuidFP))
       False -> Left <$> getErrors ret (SQLSTMTRef hstmtP)

typeMismatch :: (Applicative m) => [SQLType] -> ColDescriptor -> m (Either SQLErrors a)
typeMismatch expTys col =
  let emsg = case expTys of
        [e] -> "Expected a type: " <> show e
        es  -> "Expected one of types: " <> show es
      msg = emsg <> ", but got a type: " <> show colType <> colMsg <> hintMsg
      hintMsg = "  HINT: " <> show colType <> " is mapped to the following " <> show matches
      colType = colDataType col
      matches = HM.foldlWithKey' (\a k v -> case colType `elem` v of
                                     True -> k : a
                                     False -> a
                                 ) [] sqlMapping
      colMsg = case T.unpack (colName col) of
        "" -> ""
        _  -> ", in a column: " <> T.unpack (colName col)
      se = SQLError { sqlState = ""
                    , sqlMessage = T.pack msg
                    , sqlReturn = -1
                    }
  in  pure (Left (SQLErrors [ se ]))
                      
inQuotes :: Builder -> Builder
inQuotes b = quote `mappend` b `mappend` quote
  where quote = BSB.char8 '\''  

#if  !MIN_VERSION_GLASGOW_HASKELL(8,2,0,0)
newtype CBool = CBool Word8
  deriving (Num, Eq, Storable)
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

pattern SQL_ATTR_ACCESS_MODE :: ConnectAttr
pattern SQL_ATTR_ACCESS_MODE <- ((ConnectAttr [C.pure| int {SQL_ATTR_ACCESS_MODE} |] ==) -> True) where
  SQL_ATTR_ACCESS_MODE = ConnectAttr [C.pure| int {SQL_ATTR_ACCESS_MODE} |]

pattern SQL_ATTR_AUTOCOMMIT :: ConnectAttr
pattern SQL_ATTR_AUTOCOMMIT <- ((ConnectAttr [C.pure| int {SQL_ATTR_AUTOCOMMIT} |] ==) -> True) where
  SQL_ATTR_AUTOCOMMIT = ConnectAttr [C.pure| int {SQL_ATTR_AUTOCOMMIT} |]

pattern SQL_ATTR_CONNECTION_TIMEOUT :: ConnectAttr
pattern SQL_ATTR_CONNECTION_TIMEOUT <- ((ConnectAttr [C.pure| int {SQL_ATTR_CONNECTION_TIMEOUT} |] ==) -> True) where
  SQL_ATTR_CONNECTION_TIMEOUT = ConnectAttr [C.pure| int {SQL_ATTR_CONNECTION_TIMEOUT} |]

pattern SQL_ATTR_CURRENT_CATALOG :: ConnectAttr
pattern SQL_ATTR_CURRENT_CATALOG <- ((ConnectAttr [C.pure| int {SQL_ATTR_CURRENT_CATALOG} |] ==) -> True) where
  SQL_ATTR_CURRENT_CATALOG = ConnectAttr [C.pure| int {SQL_ATTR_CURRENT_CATALOG} |]

pattern SQL_ATTR_DISCONNECT_BEHAVIOR :: ConnectAttr
pattern SQL_ATTR_DISCONNECT_BEHAVIOR <- ((ConnectAttr [C.pure| int {SQL_ATTR_DISCONNECT_BEHAVIOR} |] ==) -> True) where
  SQL_ATTR_DISCONNECT_BEHAVIOR = ConnectAttr [C.pure| int {SQL_ATTR_DISCONNECT_BEHAVIOR} |]

pattern SQL_ATTR_ENLIST_IN_DTC :: ConnectAttr
pattern SQL_ATTR_ENLIST_IN_DTC <- ((ConnectAttr [C.pure| int {SQL_ATTR_ENLIST_IN_DTC} |] ==) -> True) where
  SQL_ATTR_ENLIST_IN_DTC = ConnectAttr [C.pure| int {SQL_ATTR_ENLIST_IN_DTC} |]

pattern SQL_ATTR_ENLIST_IN_XA :: ConnectAttr
pattern SQL_ATTR_ENLIST_IN_XA <- ((ConnectAttr [C.pure| int {SQL_ATTR_ENLIST_IN_XA} |] ==) -> True) where
  SQL_ATTR_ENLIST_IN_XA = ConnectAttr [C.pure| int {SQL_ATTR_ENLIST_IN_XA} |]

pattern SQL_ATTR_LOGIN_TIMEOUT :: ConnectAttr
pattern SQL_ATTR_LOGIN_TIMEOUT <- ((ConnectAttr [C.pure| int {SQL_ATTR_LOGIN_TIMEOUT} |] ==) -> True) where
  SQL_ATTR_LOGIN_TIMEOUT = ConnectAttr [C.pure| int {SQL_ATTR_LOGIN_TIMEOUT} |]

pattern SQL_ATTR_ODBC_CURSORS :: ConnectAttr
pattern SQL_ATTR_ODBC_CURSORS <- ((ConnectAttr [C.pure| int {SQL_ATTR_ODBC_CURSORS} |] ==) -> True) where
  SQL_ATTR_ODBC_CURSORS = ConnectAttr [C.pure| int {SQL_ATTR_ODBC_CURSORS} |]

pattern SQL_ATTR_PACKET_SIZE :: ConnectAttr
pattern SQL_ATTR_PACKET_SIZE <- ((ConnectAttr [C.pure| int {SQL_ATTR_PACKET_SIZE} |] ==) -> True) where
  SQL_ATTR_PACKET_SIZE = ConnectAttr [C.pure| int {SQL_ATTR_PACKET_SIZE} |]

pattern SQL_ATTR_QUIET_MODE :: ConnectAttr
pattern SQL_ATTR_QUIET_MODE <- ((ConnectAttr [C.pure| int {SQL_ATTR_QUIET_MODE} |] ==) -> True) where
  SQL_ATTR_QUIET_MODE = ConnectAttr [C.pure| int {SQL_ATTR_QUIET_MODE} |]

pattern SQL_ATTR_TRACE :: ConnectAttr
pattern SQL_ATTR_TRACE <- ((ConnectAttr [C.pure| int {SQL_ATTR_TRACE} |] ==) -> True) where
  SQL_ATTR_TRACE = ConnectAttr [C.pure| int {SQL_ATTR_TRACE} |]

pattern SQL_ATTR_TRACEFILE :: ConnectAttr
pattern SQL_ATTR_TRACEFILE <- ((ConnectAttr [C.pure| int {SQL_ATTR_TRACEFILE} |] ==) -> True) where
  SQL_ATTR_TRACEFILE = ConnectAttr [C.pure| int {SQL_ATTR_TRACEFILE} |]

pattern SQL_ATTR_TRANSLATE_LIB :: ConnectAttr
pattern SQL_ATTR_TRANSLATE_LIB <- ((ConnectAttr [C.pure| int {SQL_ATTR_TRANSLATE_LIB} |] ==) -> True) where
  SQL_ATTR_TRANSLATE_LIB = ConnectAttr [C.pure| int {SQL_ATTR_TRANSLATE_LIB} |]

pattern SQL_ATTR_TRANSLATE_OPTION :: ConnectAttr
pattern SQL_ATTR_TRANSLATE_OPTION <- ((ConnectAttr [C.pure| int {SQL_ATTR_TRANSLATE_OPTION} |] ==) -> True) where
  SQL_ATTR_TRANSLATE_OPTION = ConnectAttr [C.pure| int {SQL_ATTR_TRANSLATE_OPTION} |]

pattern SQL_ATTR_TXN_ISOLATION :: ConnectAttr
pattern SQL_ATTR_TXN_ISOLATION <- ((ConnectAttr [C.pure| int {SQL_ATTR_TXN_ISOLATION} |] ==) -> True) where
  SQL_ATTR_TXN_ISOLATION = ConnectAttr [C.pure| int {SQL_ATTR_TXN_ISOLATION} |]

pattern SQL_ATTR_METADATA_ID :: ConnectAttr
pattern SQL_ATTR_METADATA_ID <- ((ConnectAttr [C.pure| int {SQL_ATTR_METADATA_ID} |] ==) -> True) where
  SQL_ATTR_METADATA_ID = ConnectAttr [C.pure| int {SQL_ATTR_METADATA_ID} |]

pattern SQL_ATTR_AUTO_IPD :: ConnectAttr
pattern SQL_ATTR_AUTO_IPD <- ((ConnectAttr [C.pure| int {SQL_ATTR_AUTO_IPD} |] ==) -> True) where
  SQL_ATTR_AUTO_IPD = ConnectAttr [C.pure| int {SQL_ATTR_AUTO_IPD} |]

pattern SQL_ATTR_ASYNC_ENABLE :: ConnectAttr
pattern SQL_ATTR_ASYNC_ENABLE <- ((ConnectAttr [C.pure| int {SQL_ATTR_ASYNC_ENABLE} |] ==) -> True) where
  SQL_ATTR_ASYNC_ENABLE = ConnectAttr [C.pure| int {SQL_ATTR_ASYNC_ENABLE} |]
  
pattern SQL_ATTR_ASYNC_DBC_FUNCTIONS_ENABLE :: ConnectAttr
#ifdef mingw32_HOST_OS
pattern SQL_ATTR_ASYNC_DBC_FUNCTIONS_ENABLE <- ((const True) -> False)
# else
pattern SQL_ATTR_ASYNC_DBC_FUNCTIONS_ENABLE <- ((ConnectAttr [C.pure| int {SQL_ATTR_ASYNC_DBC_FUNCTIONS_ENABLE} |] ==) -> True) where
  SQL_ATTR_ASYNC_DBC_FUNCTIONS_ENABLE = ConnectAttr [C.pure| int {SQL_ATTR_ASYNC_DBC_FUNCTIONS_ENABLE} |]
#endif  

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
   SQL_ATTR_ACCESS_MODE
 , SQL_ATTR_AUTOCOMMIT
 , SQL_ATTR_CONNECTION_TIMEOUT
 , SQL_ATTR_CURRENT_CATALOG
 , SQL_ATTR_DISCONNECT_BEHAVIOR
 , SQL_ATTR_ENLIST_IN_DTC
 , SQL_ATTR_ENLIST_IN_XA
 , SQL_ATTR_LOGIN_TIMEOUT
 , SQL_ATTR_LOGIN_TIMEOUT
 , SQL_ATTR_ODBC_CURSORS
 , SQL_ATTR_PACKET_SIZE
 , SQL_ATTR_QUIET_MODE
 , SQL_ATTR_TRACE
 , SQL_ATTR_TRACEFILE
 , SQL_ATTR_TRANSLATE_LIB
 , SQL_ATTR_TRANSLATE_OPTION
 , SQL_ATTR_TXN_ISOLATION
 , SQL_ATTR_ASYNC_DBC_FUNCTIONS_ENABLE
 , SQL_ATTR_ASYNC_ENABLE
 , SQL_ATTR_AUTO_IPD
 , SQL_ATTR_METADATA_ID
 :: ConnectAttr
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

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) f = getCompose . fmap f . Compose

(<**>) :: ( Applicative f
         , Applicative g
         ) => f (g (a -> b)) -> f (g a) -> f (g b)
(<**>) f = getCompose . liftA2 id (Compose f) . Compose

newtype Money = Money { getMoney :: Scientific }
              deriving (Eq)

instance Show Money where
  show (Money s) =
    formatScientific Fixed (Just 4) s

newtype SmallMoney = SmallMoney { getSmallMoney :: Scientific }
                   deriving (Eq)

instance Show SmallMoney where
  show (SmallMoney s) =
    formatScientific Fixed (Just 4) s

newtype Image = Image { getImage :: LBS.ByteString }
              deriving (Eq, Show)

sqlMapping :: HM.HashMap TypeRep [SQLType]
sqlMapping =
  HM.fromList
  [ (typeOf (undefined :: CChar)     , [SQL_VARCHAR, SQL_LONGVARCHAR, SQL_CHAR])
  , (typeOf (undefined :: CUChar)    , [SQL_VARCHAR, SQL_LONGVARCHAR, SQL_CHAR])
  , (typeOf (undefined :: CWchar)    , [SQL_VARCHAR, SQL_LONGVARCHAR, SQL_CHAR, SQL_LONGVARCHAR, SQL_WLONGVARCHAR, SQL_WVARCHAR])
  , (typeOf (undefined :: CBinary)   , [SQL_LONGVARBINARY, SQL_VARBINARY])  
  , (typeOf (undefined :: CMoney)    , [SQL_DECIMAL])
  , (typeOf (undefined :: CUTinyInt) , [SQL_TINYINT])
  , (typeOf (undefined :: CTinyInt)  , [SQL_TINYINT])
  , (typeOf (undefined :: CLong)     , [SQL_INTEGER])
  , (typeOf (undefined :: CULong)    , [SQL_INTEGER])
  , (typeOf (undefined :: CSmallInt) , [SQL_SMALLINT])
  , (typeOf (undefined :: CUSmallInt), [SQL_SMALLINT])
  , (typeOf (undefined :: CFloat)    , [SQL_REAL])
  , (typeOf (undefined :: CDouble)   , [SQL_FLOAT])
  , (typeOf (undefined :: CBool)     , [SQL_BIT])
  , (typeOf (undefined :: CDate)     , [SQL_DATE, SQL_TYPE_DATE])
  , (typeOf (undefined :: CBigInt)   , [SQL_BIGINT])
  , (typeOf (undefined :: CUBigInt)  , [SQL_BIGINT])
  , (typeOf (undefined :: CTimeOfDay), [SQL_TIME, SQL_SS_TIME2])
  , (typeOf (undefined :: CLocalTime), [SQL_TIMESTAMP, SQL_TYPE_TIMESTAMP])
  , (typeOf (undefined :: CZonedTime), [SQL_SS_TIMESTAMPOFFSET])
  , (typeOf (undefined :: UUID)      , [SQL_GUID])
  ]
  
  
                
{-

- segfault issue
- right associativeness of <>
- sized variants
- extractWith errors

-}
