{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
module Database.MsSQL.Connection where

{-
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Language.C.Inline as C
--import qualified Language.C.Inline.Unsafe as UnsafeC
--import qualified Language.C.Types as C
import Foreign.Storable
import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc (free, alloca)
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as MSV
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Coerce
import Database.MsSQL.Internal.Ctx
import Database.MsSQL.Internal.SQLError
import Data.Text.Foreign as T
import qualified Data.Text as T
import Data.IORef
import Data.Word
import Data.Int
import Data.Profunctor
import GHC.Generics
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Data.Typeable
import Data.Time
import Data.UUID.Types
import Database.MsSQL.Internal.SQLTypes
import Data.Functor.Identity
import Control.Monad
import Data.Semigroup

data ConnectInfo = ConnectInfo
  { connectionString :: T.Text
  , autoCommit :: Bool
  , ansi :: Bool
  , timeout :: Int
  , readOnly :: Bool
  , attrBefore :: SV.Vector ConnectAttr
  } deriving (Show, Eq)

newtype ConnectAttr = ConnectAttr { getConnectAttr :: C.CInt}
                          deriving (Show, Read, Eq, Storable)


data SQLHENV
data SQLHDBC
data SQLHSTMT
data SQLHANDLE

newtype ColPos = ColPos (IORef CShort)

data HSTMT a = HSTMT
  { getHSTMT :: Ptr SQLHSTMT
  , colPos   :: ColPos
  } 

C.context $ mssqlCtx
  [ ("SQLWCHAR", [t|CWchar|])
  , ("SQLCHAR", [t|CChar|])
  , ("SQLHANDLE", [t|Ptr SQLHANDLE|])
  , ("SQLHENV" , [t|Ptr SQLHENV|])
  , ("SQLHDBC" , [t|Ptr SQLHDBC|])
  , ("SQLHSTMT" , [t|Ptr SQLHSTMT|])
  , ("SQLSMALLINT", [t|CShort|])
  , ("SQLREAL", [t|CFloat|])
  , ("SQLUINTEGER", [t|CULong|])
  , ("SQLINTEGER", [t|CLong|])
  , ("SQLLEN", [t|CLong|])
  , ("SQL_DATE_STRUCT", [t|CDate|])
  , ("SQL_TIME_STRUCT", [t|CTimeOfDay|])
  , ("SQL_TIMESTAMP_STRUCT", [t|CLocalTime|])
  , ("SQLGUID", [t|UUID|])
  ]

C.verbatim "#define UNICODE"

#ifdef mingw32_HOST_OS
C.include "<windows.h>"
#endif
C.include "<stdio.h>"
C.include "<stdlib.h>"
--C.include "<sql.h>"
C.include "<sqlext.h>"
C.include "<sqltypes.h>"
C.include "<sqlucode.h>"
--C.include "msodbcsql.h"


odbcVersion :: IO Int --ByteString
odbcVersion = fromIntegral <$> [C.block|int{return SQL_ATTR_ODBC_VERSION;}|]


connectInfo :: T.Text -> ConnectInfo
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

newtype Session a = Session { runSession :: ReaderT Connection (ExceptT SQLErrors IO) a}
                  deriving (Functor, Applicative, Monad)


connect :: ConnectInfo -> IO (Either SQLErrors Connection)
connect connInfo = do
  alloca $ \(henvp :: Ptr (Ptr SQLHENV)) -> do
    alloca $ \(hdbcp :: Ptr (Ptr SQLHDBC)) -> do
      doConnect henvp hdbcp
  where
    doConnect henvp hdbcp = do
      (ctxt, i16) <- asForeignPtr $ connectionString connInfo
      let ctxtLen = fromIntegral i16 :: C.CInt

      let HandleType handleType = SQL_HANDLE_DBC
      ret <- [C.block| int {
        SQLRETURN ret;
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
        if (!SQL_SUCCEEDED(ret)) return ret;

        return 0;
        }|]

      henv <- peek henvp
      hdbc <- peek hdbcp
      case ret of
        0 -> do
          mE <- getMessages $ SQLDBCRef hdbc
          print mE
          pure $ Right $ Connection
                           { _henv = henv
                           , _hdbc = hdbc
                           }
        _ -> do
          msgE <- getMessages $ SQLDBCRef hdbc
          case msgE of
            Left e -> pure $ Left []
            Right es -> pure $ Left []

disconnect :: Connection -> IO ()
disconnect con = do
  ret <- [C.block| int {
    SQLRETURN ret;
    SQLHENV henv = $(SQLHENV henv);
    SQLHDBC hdbc = $(SQLHDBC hdbc);

    SQLWCHAR eMSG [SQL_MAX_MESSAGE_LENGTH];
    SQLSMALLINT eMSGLen;
    
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

  }|]

  pure ()
  where
    hdbc = _hdbc con
    henv = _henv con

data HandleRef
  = SQLENVRef (Ptr SQLHENV)
  | SQLDBCRef (Ptr SQLHDBC)
  
getMessages :: HandleRef -> IO (Either SQLError [(T.Text, T.Text)])
getMessages handleRef = do
  msgsRef <- newIORef []
  appendMessage <- appendMessageM msgsRef
  let
    (HandleType handleType, handle, handleName) = case handleRef of
      SQLENVRef h -> (SQL_HANDLE_ENV, castPtr h, "ENV")
      SQLDBCRef h -> (SQL_HANDLE_DBC, castPtr h, "DATABASE")
  ret <- [C.block| int {
             SQLRETURN ret = 0;
             SQLINTEGER i = 0;
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

sqldirect :: Connection -> Ptr SQLHSTMT -> T.Text -> IO ()
sqldirect con hstmt sql = do
  (queryWStr, queryLen) <- fmap (fmap fromIntegral) $ asForeignPtr $ sql
  numResultColsFP :: ForeignPtr CShort <- mallocForeignPtr

--  mallocForeignPtrArray 1024
  res <- [C.block| int {
    SQLRETURN ret = 0;
    SQLHENV henv = $(SQLHENV henv);
    SQLHDBC hdbc = $(SQLHDBC hdbc);
    SQLHSTMT hstmt = $(SQLHSTMT hstmt);
    SQLSMALLINT* numColumnPtr = $fptr-ptr:(SQLSMALLINT* numResultColsFP);

    ret = SQLExecDirectW(hstmt, $fptr-ptr:(SQLWCHAR* queryWStr), $(int queryLen));
    if (!SQL_SUCCEEDED(ret)) return ret;

    ret = SQLNumResultCols(hstmt, numColumnPtr);
    if (!SQL_SUCCEEDED(ret)) return ret;

    return ret;
    }|]
  numResultCols <- withForeignPtr numResultColsFP peek
  baseTableNamesFP :: ForeignPtr Word8 <- mallocForeignPtrArray (fromIntegral numResultCols * 1024)
  res <- [C.block| int {
    SQLRETURN ret = 0;
    SQLSMALLINT* numColumnPtr = $fptr-ptr:(SQLSMALLINT* numResultColsFP);
    SQLHSTMT hstmt = $(SQLHSTMT hstmt);
//    char* 

    for (int i = 0 ; i < *numColumnPtr ; i++ ) {
    }

    return ret;
    }|]

  pure ()
  where
    hdbc = _hdbc con
    henv = _henv con
        
allocHSTMT :: Connection -> IO (Either SQLErrors (HSTMT a))
allocHSTMT con = do
  alloca $ \(hstmtp :: Ptr (Ptr SQLHSTMT)) -> do
    ret <- [C.block| SQLRETURN {
      SQLRETURN ret = 0;
      SQLHSTMT* hstmtp = $(SQLHSTMT* hstmtp);
      SQLHDBC hdbc = $(SQLHDBC hdbc);
      ret = SQLAllocHandle(SQL_HANDLE_STMT, hdbc, hstmtp);
      return ret;
    }|]
    case isSuccessful $ ResIndicator $ fromIntegral ret of
      False -> do
        msgsE <- getMessages (SQLDBCRef hdbc)
        print msgsE
        pure $ Left []
      True -> do
        hstmt <- peek hstmtp
        cpos <- initColPos
        pure $ Right $ HSTMT hstmt cpos
  where
    hdbc = _hdbc con


releaseHSTMT :: HSTMT a -> IO ()
releaseHSTMT stmt = do
  res <- [C.block| SQLRETURN {
      SQLRETURN ret = 0;
      SQLHSTMT hstmt =  $(SQLHSTMT hstmt);
      if (hstmt != SQL_NULL_HSTMT) {
        ret = SQLFreeHandle(SQL_HANDLE_STMT, hstmt);
      }
      return ret;
  }|]
  pure ()
  where
    hstmt = getHSTMT stmt

withHSTMT :: Connection -> (HSTMT a -> IO (Either SQLErrors a)) -> IO (Either SQLErrors a)
withHSTMT con act = do
  hstmtE <- allocHSTMT con
  case hstmtE of
    Right hstmt -> do
      r <- act hstmt
      releaseHSTMT hstmt
      pure r
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
sqlGetInfo :: Connection -> InfoType -> IO T.Text
sqlGetInfo con 0  = do
  (infoFP :: ForeignPtr Word16) <- mallocForeignPtrBytes (16 * 1024)
  (bufferSizeOut :: ForeignPtr Int) <- mallocForeignPtr
  ret <- [C.block| SQLRETURN {
      SQLRETURN ret = 0;
      SQLHDBC hdbc = $(SQLHDBC hdbc);
      ret = SQLGetInfo(hdbc, SQL_DATABASE_NAME, $fptr-ptr:(SQLWCHAR* infoFP), (SQLSMALLINT)(16 * 1024), $fptr-ptr:(SQLSMALLINT * bufferSizeOut));
      return ret;
  }|]
  bufferSize <- withForeignPtr bufferSizeOut peek
  info <- withForeignPtr infoFP $ \infoP -> fromPtr infoP (round ((fromIntegral bufferSize :: Double)/2))
  pure info
  where
    hdbc = _hdbc con
sqlGetInfo con _  = do
  (infoFP :: ForeignPtr CULong) <- mallocForeignPtr
  res <- withForeignPtr infoFP $ \infoP -> do
    ret <- [C.block| SQLRETURN {
             SQLRETURN ret = 0;
             SQLHDBC hdbc = $(SQLHDBC hdbc);
             ret = SQLGetInfo(hdbc, SQL_MAX_CONCURRENT_ACTIVITIES, $(SQLUINTEGER* infoP), (SQLSMALLINT)(sizeof(SQLUINTEGER)), NULL);
             return ret;
           }|]
    peek infoP
  pure $ T.pack $ show res
  where
    hdbc = _hdbc con


--sqlCancel :: HSTMT a -> IO ()

{-
data Row f (cols :: [*]) where
  RowNil  :: Row f '[]
  RowCons :: f c -> Row f cs -> Row f (c ': cs)

freeBuffer :: Row f cols -> IO ()
freeBuffer RowNil = pure ()
freeBuffer (RowCons c cs) = do
  freeBuffer cs
-}

isSuccessful :: ResIndicator -> Bool
isSuccessful SQL_SUCCESS           = True
isSuccessful SQL_SUCCESS_WITH_INFO = True
isSuccessful _                     = False


extractVal :: Storable t => ColBuffer t -> IO t
extractVal cbuff = withForeignPtr (getColBuffer cbuff) peek

getLengthOrIndicator :: ColBuffer t -> IO (Either CLong ResIndicator)
getLengthOrIndicator cb = do
  lenOrInd <- fromIntegral <$> (peekFP $ lengthOrIndicatorFP cb)
  pure $ case lenOrInd `elem` [SQL_NULL_DATA, SQL_NO_TOTAL] of
    True -> Right lenOrInd
    False -> Left $ coerce lenOrInd
                                                             

data ColBuffer t = ColBuffer
  { getColBuffer :: ForeignPtr t
  , lengthOrIndicatorFP :: ForeignPtr CLong 
  }
  
fetchRows :: HSTMT a -> IO r -> IO (Vector r)
fetchRows hstmt rowP = flip V.unfoldrM () $ \_ -> do
  res <- sqlFetch hstmt
  case res of
    0 -> do
      r <- rowP
      pure $ Just (r, ())
    _ -> pure Nothing

sqlRowCount :: HSTMT a -> IO (Either SQLErrors Int64)
sqlRowCount stmt = do
  (rcountFP :: ForeignPtr CLong) <- mallocForeignPtr
  res <- withForeignPtr rcountFP $ \rcountP -> do
    [C.block| int {
        SQLRETURN ret = 0;
        SQLHSTMT hstmt = $(SQLHSTMT hstmt);

        ret = SQLRowCount(hstmt, $(SQLLEN* rcountP));

        return ret;
    }|]
  case res of
    0 -> (Right . fromIntegral) <$> peekFP rcountFP
    _ -> pure $ Left []
  where
    hstmt = getHSTMT stmt


data FieldDescriptor t = FieldDescriptor

initColPos :: IO ColPos
initColPos = ColPos <$> newIORef 1

nextColPos :: HSTMT a -> IO CShort
nextColPos hstmt = let (ColPos wref) = colPos hstmt
                   in atomicModifyIORef' wref (\w -> (w +1, w)) 

type Query = T.Text

query :: forall r.(FromRow r, SQLBindCol (RowBufferType r)) => Connection -> Query -> IO (Either SQLErrors (Vector r))
query con q = do
  let rowPFun = (getValue $ runRowParser fromRow)
  withHSTMT con $ \hstmt -> do
    sqldirect con (getHSTMT hstmt) q
    colBufferE <- sqlBindCol (undefined :: FieldDescriptor (RowBufferType r)) hstmt
    case colBufferE of
      Right colBuffer -> Right <$> fetchRows hstmt (rowPFun colBuffer)
      Left e -> pure $ Left e

execute :: Connection -> Query -> IO (Either SQLErrors Int64)
execute con q = do
  withHSTMT con $ \hstmt -> do
    sqldirect con (getHSTMT hstmt) q
    sqlRowCount hstmt
  
data Value a b = Value { getValue :: a -> b}


instance Functor (Value a) where
  fmap f (Value fn) = Value (f . fn)

instance Applicative (Value a) where
  pure a                  = Value (\_ -> a)
  (Value f) <*> (Value a) = Value (f <*> a)

instance Profunctor Value where
  rmap f (Value fn) = Value (f . fn)
  lmap f (Value fn) = Value (fn . f)

newtype RowParser rowbuff t = RowParser { runRowParser :: Value rowbuff (IO t) }

instance Profunctor RowParser where
  rmap f rp = fmap f rp
  lmap f rp = RowParser $ lmap f $ runRowParser rp

instance Functor (RowParser rowbuf) where
  fmap f (RowParser (Value fn)) = RowParser $ Value $ \inp -> do
    res <- fn inp
    pure (f res)

instance Applicative (RowParser rowbuf) where
  pure a  = RowParser $ Value $ const $ pure a
  (RowParser (Value f)) <*> (RowParser (Value a)) = RowParser $ Value $ \inp -> do
    fn <- f inp
    v <- a inp
    pure (fn v)
    
    
class FromRow t where
  type RowBufferType t :: *
  type RowBufferType t = GFieldBufferType (Rep t) ()
  fromRow :: RowParser (RowBufferType t) t
  default fromRow :: ( Generic t
                     , GFromRow (Rep t)
                     , (RowBufferType t ~ GFieldBufferType (Rep t) ())) => RowParser (RowBufferType t) t
  fromRow = dimap id to gFromRow

type family GFieldBufferType (f :: * -> *)  :: * -> * where
  GFieldBufferType (M1 c i f) = M1 c i (GFieldBufferType f)
  GFieldBufferType (f :*: g)  = (GFieldBufferType f) :*: (GFieldBufferType g)
  GFieldBufferType (K1 k a)   = K1 k (ColBuffer (FieldBufferType a))
  
class GFromRow (f :: * -> *) where
  gFromRow :: RowParser ((GFieldBufferType f) ()) (f a)

instance (GFromRow f) => GFromRow (M1 c i f) where
  gFromRow = dimap (\(M1 a) -> a) M1 gFromRow 

instance GFromRow U1 where
  gFromRow = pure U1

instance (GFromRow f, GFromRow g) => GFromRow (f :*: g) where
  gFromRow = ((:*:) <$> (lmap (\(f :*: _) -> f) gFromRow)
                    <*> (lmap (\(_ :*: g) -> g) gFromRow))

instance (FromField a) => GFromRow (K1 k a) where
  gFromRow =  dimap (\(K1 a) -> a) K1 $ RowParser (fromField :: FieldParser a)
    
instance (FromField a, FromField b) => FromRow (a, b) where
  type RowBufferType (a, b) = (ColBuffer (FieldBufferType a), ColBuffer (FieldBufferType b))
  fromRow = (,) <$> (field fst) <*> (field snd)

instance FromField a => FromRow (Identity a) where
  type RowBufferType (Identity a) = ColBuffer (FieldBufferType a)
  fromRow = Identity <$> (field id)

type FieldParser t = Value (ColBuffer (FieldBufferType t)) (IO t)

class FromField t where
  type FieldBufferType t :: *
  fromField :: Value (ColBuffer (FieldBufferType t)) (IO t)

instance FromField Int where
  type FieldBufferType Int = CInt
  fromField = Value $ \i -> extractVal i >>= (\v -> pure $ fromIntegral v)

instance FromField Word8 where
  type FieldBufferType Word8 = CUChar
  fromField = Value $ \i -> extractVal i >>= (\v -> pure $ fromIntegral v)
  
instance FromField Int16 where
  type FieldBufferType Int16 = CShort
  fromField = Value $ \i -> extractVal i >>= (\v -> pure $ fromIntegral v)

instance FromField Int64 where
  type FieldBufferType Int64 = CLLong
  fromField = Value $ \i -> extractVal i >>= (\v -> pure $ coerce v)  

instance FromField Bool where
  type FieldBufferType Bool = CBool
  fromField = Value $ \i -> extractVal i >>= (\v -> pure $ if v == 1 then True else False)  

instance FromField Date where
  type FieldBufferType Date = CFloat
  fromField = Value $ \i -> extractVal i >>= (\v -> pure $ Date $ round v)

instance FromField ByteString where
  type FieldBufferType ByteString = CChar
  fromField = Value $ \v -> do
    lengthOrIndicator <- peekFP $ lengthOrIndicatorFP v
    print (66666666666666666666666666666, lengthOrIndicator)
    withForeignPtr (getColBuffer v) $ \ccharP -> BS.packCStringLen (ccharP, fromIntegral lengthOrIndicator)


instance FromField T.Text where
  type FieldBufferType T.Text = CWchar
  fromField = Value $ \v -> do
    lengthOrIndicator <- peekFP $ lengthOrIndicatorFP v
    print (66666666666666666666666666666, lengthOrIndicator)
    let clen = round ((fromIntegral lengthOrIndicator :: Double)/2)
    withForeignPtr (castForeignPtr $ getColBuffer v) $ \cwcharP -> T.fromPtr cwcharP (fromIntegral clen)

instance FromField Day where
  type FieldBufferType Day = CDate
  fromField = Value $ \i -> extractVal i >>= (\v -> pure $ getDate v)

instance FromField TimeOfDay where
  type FieldBufferType TimeOfDay = CTimeOfDay
  fromField = Value $ \i -> extractVal i >>= (\v -> pure $ getTimeOfDay v)

instance FromField LocalTime where
  type FieldBufferType LocalTime = CLocalTime
  fromField = Value $ \i -> extractVal i >>= (\v -> pure $ getLocalTime v)

instance FromField UTCTime where
  type FieldBufferType UTCTime = FieldBufferType ByteString
  fromField = Value $ \v -> toUTC <$> (getValue fromField v)
    where toUTC :: ByteString -> UTCTime
          toUTC = undefined -- parseTime defaultTimeLocale ""

instance FromField UUID where
  type FieldBufferType UUID = UUID
  fromField = Value $ \i -> extractVal i
            
instance FromField a => FromField (Maybe a) where
  type FieldBufferType (Maybe a) = FieldBufferType a
  fromField = Value $ \v -> do
    lengthOrIndicator <- peekFP $ lengthOrIndicatorFP v
    if lengthOrIndicator == fromIntegral SQL_NULL_DATA -- TODO: Only long worked not SQLINTEGER
      then pure Nothing
      else Just <$> ((getValue fromField) v )


field :: FromField f => (a -> ColBuffer (FieldBufferType f)) -> RowParser a f
field f = RowParser $ lmap f $ fromField


int :: (a -> Int) -> Value a Int
int f = lmap f $ Value id

float :: (a -> Float) -> Value a Float
float f = lmap f $ Value id

newtype Date = Date Int
             deriving (Show, Num)

date :: (a -> String) -> Value a Date
date f = lmap (Date . read . f) $ Value id

coerceColBuffer :: (Coercible a b) => ColBuffer a -> ColBuffer b
coerceColBuffer c = c {getColBuffer  = castForeignPtr $ getColBuffer c}

peekFP :: Storable t => ForeignPtr t -> IO t
peekFP fp = withForeignPtr fp peek

type SQLBindColM t = ReaderT ColPos IO (Either SQLErrors t)

class SQLBindCol t where
  sqlBindCol :: FieldDescriptor t -> HSTMT a -> IO (Either SQLErrors t)

instance SQLBindCol (ColBuffer CChar) where
  sqlBindCol _fdesc hstmt = do
    let hstmtP = getHSTMT hstmt
    chrFP <- mallocForeignPtrBytes 100 -- TODO: Get the length from col prop
    cpos <- nextColPos hstmt
    lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
    res <- withForeignPtr chrFP $ \chrp -> do
      [C.block| int {
        SQLRETURN ret = 0;
        SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
        SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
        ret = SQLBindCol(hstmt, $(SQLSMALLINT cpos), SQL_C_CHAR, $(SQLCHAR* chrp), 100, lenOrInd);
        return ret;
      }|]
    case res of
      0 -> pure $ Right (ColBuffer chrFP lenOrIndFP)
      _ -> pure $ Left []

instance SQLBindCol (ColBuffer CWchar) where
  sqlBindCol _fdesc hstmt = do
    let hstmtP = getHSTMT hstmt
    txtFP <- mallocForeignPtrBytes 100 -- TODO: Get the length from col prop
    cpos <- nextColPos hstmt
    lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
    res <- withForeignPtr txtFP $ \txtP -> do
      [C.block| int {
        SQLRETURN ret = 0;
        SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
        SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
        ret = SQLBindCol(hstmt, $(SQLSMALLINT cpos), SQL_C_WCHAR, $(SQLWCHAR* txtP), 100, lenOrInd);
        return ret;
      }|]
    case res of
      0 -> pure $ Right (ColBuffer txtFP lenOrIndFP)
      _ -> pure $ Left []
    
instance SQLBindCol (ColBuffer CInt) where
  sqlBindCol _fdesc hstmt = do
   let hstmtP = getHSTMT hstmt
   intFP <- mallocForeignPtr
   cpos <- nextColPos hstmt
   lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
   res <- withForeignPtr intFP $ \intP -> do
    [C.block| int {
        SQLRETURN ret = 0;
        SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
        SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
        ret = SQLBindCol(hstmt, $(SQLSMALLINT cpos), SQL_C_LONG, $(int* intP), sizeof(SQLINTEGER), lenOrInd);
        if (-2 == SQL_NULL_DATA) printf("NULLLLLLLLLLLLL"); else printf("ELSEEEEEEEEEEEEEEEEEEEEEEEEE");
        return ret;
    }|]
   case res of
     0 -> pure $ Right (ColBuffer intFP lenOrIndFP)
     _ -> pure $ Left []

instance SQLBindCol (ColBuffer CShort) where
  sqlBindCol _fdesc hstmt = do
   let hstmtP = getHSTMT hstmt
   shortFP <- mallocForeignPtr
   cpos <- nextColPos hstmt
   lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
   res <- withForeignPtr shortFP $ \shortP -> do
    [C.block| int {
        SQLRETURN ret = 0;
        SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
        SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
        ret = SQLBindCol(hstmt, $(SQLSMALLINT cpos), SQL_C_SHORT, $(SQLSMALLINT* shortP), sizeof(SQLSMALLINT), lenOrInd);
        return ret;
    }|]
   case res of
     0 -> pure $ Right (ColBuffer shortFP lenOrIndFP)
     _ -> pure $ Left []

instance SQLBindCol (ColBuffer CFloat) where
  sqlBindCol _fdesc hstmt = do
   let hstmtP = getHSTMT hstmt
   floatFP <- mallocForeignPtr
   cpos <- nextColPos hstmt
   lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
   res <- withForeignPtr floatFP $ \floatP -> do
    [C.block| int {
        SQLRETURN ret = 0;
        SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
        SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
        ret = SQLBindCol(hstmt, $(SQLSMALLINT cpos), SQL_C_FLOAT, $(SQLREAL* floatP), sizeof(SQLREAL), lenOrInd);
        return ret;
    }|]
   case res of
     0 -> pure $ Right (ColBuffer floatFP lenOrIndFP)
     _ -> pure $ Left []

instance SQLBindCol (ColBuffer CBool) where
  sqlBindCol _fdesc hstmt = do
   let hstmtP = getHSTMT hstmt
   chrFP <- mallocForeignPtr
   cpos <- nextColPos hstmt
   lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
   res <- withForeignPtr chrFP $ \chrP -> do
    [C.block| int {
        SQLRETURN ret = 0;
        SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
        SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
        ret = SQLBindCol(hstmt, $(SQLSMALLINT cpos), SQL_C_BIT, $(SQLCHAR* chrP), sizeof(SQLCHAR), lenOrInd);
        return ret;
    }|]
   case res of
     0 -> pure $ Right (ColBuffer (castForeignPtr chrFP) lenOrIndFP)
     _ -> pure $ Left []

instance SQLBindCol (ColBuffer CUChar) where
  sqlBindCol _fdesc hstmt = do
   let hstmtP = getHSTMT hstmt
   chrFP <- mallocForeignPtr
   cpos <- nextColPos hstmt
   lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
   res <- withForeignPtr chrFP $ \chrP -> do
    [C.block| int {
        SQLRETURN ret = 0;
        SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
        SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
        ret = SQLBindCol(hstmt, $(SQLSMALLINT cpos), SQL_C_TINYINT, $(SQLCHAR* chrP), sizeof(SQLCHAR), lenOrInd);
        return ret;
    }|]
   case res of
     0 -> pure $ Right (ColBuffer (castForeignPtr chrFP) lenOrIndFP)
     _ -> pure $ Left []



instance SQLBindCol (ColBuffer CDate) where
  sqlBindCol _fdesc hstmt = do
   let hstmtP = getHSTMT hstmt
   dateFP <- mallocForeignPtr
   cpos <- nextColPos hstmt
   lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
   res <- withForeignPtr dateFP $ \dateP -> do
    [C.block| int {
        SQLRETURN ret = 0;
        SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
        SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
        ret = SQLBindCol(hstmt, $(SQLSMALLINT cpos), SQL_C_TYPE_DATE, $(SQL_DATE_STRUCT* dateP), sizeof(SQL_DATE_STRUCT), lenOrInd);
        return ret;
    }|]
   case res of
     0 -> pure $ Right (ColBuffer (dateFP) lenOrIndFP)
     _ -> pure $ Left []



intsz = [C.pure| int { sizeof(BYTE) } |]

instance SQLBindCol (ColBuffer CLLong) where
  sqlBindCol _fdesc hstmt = do
   let hstmtP = getHSTMT hstmt
   llongFP <- mallocForeignPtr
   cpos <- nextColPos hstmt
   lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
   res <- withForeignPtr llongFP $ \llongP -> do
    [C.block| int {
        SQLRETURN ret = 0;
        SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
        SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
        ret = SQLBindCol(hstmt, $(SQLSMALLINT cpos), SQL_C_SBIGINT, $(long long* llongP), sizeof(long long), lenOrInd);
        return ret;
    }|]
   case res of
     0 -> pure $ Right (ColBuffer llongFP lenOrIndFP)
     _ -> pure $ Left []

instance SQLBindCol (ColBuffer CTimeOfDay) where
  sqlBindCol _fdesc hstmt = do
   let hstmtP = getHSTMT hstmt
   todFP <- mallocForeignPtr
   cpos <- nextColPos hstmt
   lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
   res <- withForeignPtr todFP $ \todP -> do
    [C.block| int {
        SQLRETURN ret = 0;
        SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
        SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
        ret = SQLBindCol(hstmt, $(SQLSMALLINT cpos), SQL_C_TYPE_TIME, $(SQL_TIME_STRUCT* todP), sizeof(SQL_TIME_STRUCT), lenOrInd);
        return ret;
    }|]
   case res of
     0 -> pure $ Right (ColBuffer (todFP) lenOrIndFP)
     _ -> pure $ Left []

instance SQLBindCol (ColBuffer CLocalTime) where
  sqlBindCol _fdesc hstmt = do
   let hstmtP = getHSTMT hstmt
   ltimeFP <- mallocForeignPtr
   cpos <- nextColPos hstmt
   lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
   res <- withForeignPtr ltimeFP $ \ltimeP -> do
    [C.block| int {
        SQLRETURN ret = 0;
        SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
        SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
        ret = SQLBindCol(hstmt, $(SQLSMALLINT cpos), SQL_C_TYPE_TIMESTAMP, $(SQL_TIMESTAMP_STRUCT* ltimeP), sizeof(SQL_TIMESTAMP_STRUCT), lenOrInd);
        return ret;
    }|]
   case res of
     0 -> pure $ Right (ColBuffer (ltimeFP) lenOrIndFP)
     _ -> pure $ Left []

instance SQLBindCol (ColBuffer UUID) where
  sqlBindCol _fdesc hstmt = do
   let hstmtP = getHSTMT hstmt
   uuidFP <- mallocForeignPtr
   cpos <- nextColPos hstmt
   lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
   res <- withForeignPtr uuidFP $ \uuidP -> do
    [C.block| int {
        SQLRETURN ret = 0;
        SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
        SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
        ret = SQLBindCol(hstmt, $(SQLSMALLINT cpos), SQL_C_GUID, $(SQLGUID* uuidP), 16, lenOrInd);
        return ret;
    }|]
   case res of
     0 -> pure $ Right (ColBuffer (uuidFP) lenOrIndFP)
     _ -> pure $ Left []

instance SQLBindCol (a, b) where
  sqlBindCol _fsdec _hstmt = do
    pure $ Right undefined

instance (SQLBindCol (f a)) => SQLBindCol (M1 c i f a) where
  sqlBindCol _fdesc hstmt = (fmap M1) <$> sqlBindCol (undefined :: FieldDescriptor (f a)) hstmt

instance (SQLBindCol (f a), SQLBindCol (g a)) => SQLBindCol ((f :*: g) a) where
  sqlBindCol _fdesc hstmt = do
    fE <- sqlBindCol (undefined :: FieldDescriptor (f a)) hstmt
    gE <- sqlBindCol (undefined :: FieldDescriptor (g a)) hstmt
    pure ((:*:) <$> fE <*> gE)

instance SQLBindCol (U1 a) where
  sqlBindCol _ _ = pure $ Right U1

instance SQLBindCol a => SQLBindCol (K1 i a t) where
  sqlBindCol _fdesc hstmt = (fmap K1) <$> sqlBindCol (undefined :: FieldDescriptor a) hstmt


class GOrdT (f :: * -> *) where
  gOrdT :: f a -> String

instance (GOrdT f) => GOrdT (M1 c i f) where
  gOrdT (M1 a) = gOrdT a

instance (GOrdT f, GOrdT g) => GOrdT (f :*: g) where
  gOrdT (f :*: g) = gOrdT f ++ "-->"  ++ gOrdT g

instance Typeable a => GOrdT (K1 i a) where
  gOrdT (K1 a) = show $ typeOf a

ordT :: (Generic a, GOrdT (Rep a)) => a -> String
ordT v = gOrdT $ from v

#if  !MIN_VERSION_GLASGOW_HASKELL(8,2,0,0)
newtype CBool = CBool Word8
  deriving (Num, Eq, Storable)
#endif

newtype ResIndicator = ResIndicator C.CLong
  deriving (Show, Read, Eq, Storable, Num, Integral, Real, Enum, Ord)

pattern SQL_NULL_DATA :: ResIndicator
pattern SQL_NULL_DATA <- ((ResIndicator [C.pure| long {SQL_NULL_DATA} |] ==) -> True) where
  SQL_NULL_DATA = ResIndicator [C.pure| long {SQL_NULL_DATA} |]  

pattern SQL_NO_TOTAL :: ResIndicator
pattern SQL_NO_TOTAL <- ((ResIndicator [C.pure| long {SQL_NO_TOTAL} |] ==) -> True) where
  SQL_NO_TOTAL = ResIndicator [C.pure| long {SQL_NO_TOTAL} |]

pattern SQL_DATA_AT_EXEC :: ResIndicator
pattern SQL_DATA_AT_EXEC <- ((ResIndicator [C.pure| long {SQL_DATA_AT_EXEC} |] ==) -> True) where
  SQL_DATA_AT_EXEC = ResIndicator [C.pure| long {SQL_DATA_AT_EXEC} |]

pattern SQL_SUCCESS :: ResIndicator
pattern SQL_SUCCESS <- ((ResIndicator [C.pure| long {SQL_SUCCESS} |] ==) -> True) where
  SQL_SUCCESS = ResIndicator [C.pure| long {SQL_SUCCESS} |]

pattern SQL_SUCCESS_WITH_INFO :: ResIndicator
pattern SQL_SUCCESS_WITH_INFO <- ((ResIndicator [C.pure| long {SQL_SUCCESS_WITH_INFO} |] ==) -> True) where
  SQL_SUCCESS_WITH_INFO = ResIndicator [C.pure| long {SQL_SUCCESS_WITH_INFO} |]

pattern SQL_NO_DATA :: ResIndicator
pattern SQL_NO_DATA <- ((ResIndicator [C.pure| long {SQL_NO_DATA} |] ==) -> True) where
  SQL_NO_DATA = ResIndicator [C.pure| long {SQL_NO_DATA} |]

pattern SQL_ERROR :: ResIndicator
pattern SQL_ERROR <- ((ResIndicator [C.pure| long {SQL_ERROR} |] ==) -> True) where
  SQL_ERROR = ResIndicator [C.pure| long {SQL_ERROR} |]

pattern SQL_INVALID_HANDLE :: ResIndicator
pattern SQL_INVALID_HANDLE <- ((ResIndicator [C.pure| long {SQL_INVALID_HANDLE} |] ==) -> True) where
  SQL_INVALID_HANDLE = ResIndicator [C.pure| long {SQL_INVALID_HANDLE} |]

pattern SQL_STILL_EXECUTING :: ResIndicator
pattern SQL_STILL_EXECUTING <- ((ResIndicator [C.pure| long {SQL_STILL_EXECUTING} |] ==) -> True) where
  SQL_STILL_EXECUTING = ResIndicator [C.pure| long {SQL_STILL_EXECUTING} |]

pattern SQL_NEED_DATA :: ResIndicator
pattern SQL_NEED_DATA <- ((ResIndicator [C.pure| long {SQL_NEED_DATA} |] ==) -> True) where
  SQL_NEED_DATA = ResIndicator [C.pure| long {SQL_NEED_DATA} |]  

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
  
newtype SQLType = SQLType C.CInt
  deriving (Show, Read, Eq, Storable)

pattern SQL_UNKNOWN_TYPE :: SQLType
pattern SQL_UNKNOWN_TYPE <- ((SQLType [C.pure| int {SQL_UNKNOWN_TYPE} |] ==) -> True) where
  SQL_UNKNOWN_TYPE = SQLType [C.pure| int {SQL_UNKNOWN_TYPE} |]

pattern SQL_CHAR :: SQLType
pattern SQL_CHAR <- ((SQLType [C.pure| int {SQL_CHAR} |] ==) -> True) where
  SQL_CHAR = SQLType [C.pure| int {SQL_CHAR} |]

pattern SQL_NUMERIC :: SQLType
pattern SQL_NUMERIC <- ((SQLType [C.pure| int {SQL_NUMERIC} |] ==) -> True) where
  SQL_NUMERIC = SQLType [C.pure| int {SQL_NUMERIC} |]

pattern SQL_DECIMAL :: SQLType
pattern SQL_DECIMAL <- ((SQLType [C.pure| int {SQL_DECIMAL} |] ==) -> True) where
  SQL_DECIMAL = SQLType [C.pure| int {SQL_DECIMAL} |]  

pattern SQL_INTEGER :: SQLType
pattern SQL_INTEGER <- ((SQLType [C.pure| int {SQL_INTEGER} |] ==) -> True) where
  SQL_INTEGER = SQLType [C.pure| int {SQL_INTEGER} |]

pattern SQL_SMALLINT :: SQLType
pattern SQL_SMALLINT <- ((SQLType [C.pure| int {SQL_SMALLINT} |] ==) -> True) where
  SQL_SMALLINT = SQLType [C.pure| int {SQL_SMALLINT} |]

pattern SQL_FLOAT :: SQLType
pattern SQL_FLOAT <- ((SQLType [C.pure| int {SQL_FLOAT} |] ==) -> True) where
  SQL_FLOAT = SQLType [C.pure| int {SQL_FLOAT} |]

pattern SQL_REAL :: SQLType
pattern SQL_REAL <- ((SQLType [C.pure| int {SQL_REAL} |] ==) -> True) where
  SQL_REAL = SQLType [C.pure| int {SQL_REAL} |]

pattern SQL_DOUBLE :: SQLType
pattern SQL_DOUBLE <- ((SQLType [C.pure| int {SQL_DOUBLE} |] ==) -> True) where
  SQL_DOUBLE = SQLType [C.pure| int {SQL_DOUBLE} |]

pattern SQL_DATETIME :: SQLType
pattern SQL_DATETIME <- ((SQLType [C.pure| int {SQL_DATETIME} |] ==) -> True) where
  SQL_DATETIME = SQLType [C.pure| int {SQL_DATETIME} |]

pattern SQL_VARCHAR :: SQLType
pattern SQL_VARCHAR <- ((SQLType [C.pure| int {SQL_VARCHAR} |] ==) -> True) where
  SQL_VARCHAR = SQLType [C.pure| int {SQL_VARCHAR} |]

pattern SQL_DATE :: SQLType
pattern SQL_DATE <- ((SQLType [C.pure| int {SQL_DATE} |] ==) -> True) where
  SQL_DATE = SQLType [C.pure| int {SQL_DATE} |]

pattern SQL_INTERVAL :: SQLType
pattern SQL_INTERVAL <- ((SQLType [C.pure| int {SQL_INTERVAL} |] ==) -> True) where
  SQL_INTERVAL = SQLType [C.pure| int {SQL_INTERVAL} |]

pattern SQL_TIME :: SQLType
pattern SQL_TIME <- ((SQLType [C.pure| int {SQL_TIME} |] ==) -> True) where
  SQL_TIME = SQLType [C.pure| int {SQL_TIME} |]

pattern SQL_TIMESTAMP :: SQLType
pattern SQL_TIMESTAMP <- ((SQLType [C.pure| int {SQL_TIMESTAMP} |] ==) -> True) where
  SQL_TIMESTAMP = SQLType [C.pure| int {SQL_TIMESTAMP} |]

pattern SQL_LONGVARCHAR :: SQLType
pattern SQL_LONGVARCHAR <- ((SQLType [C.pure| int {SQL_LONGVARCHAR} |] ==) -> True) where
  SQL_LONGVARCHAR = SQLType [C.pure| int {SQL_LONGVARCHAR} |]

pattern SQL_BINARY :: SQLType
pattern SQL_BINARY <- ((SQLType [C.pure| int {SQL_BINARY} |] ==) -> True) where
  SQL_BINARY = SQLType [C.pure| int {SQL_BINARY} |]

pattern SQL_VARBINARY :: SQLType
pattern SQL_VARBINARY <- ((SQLType [C.pure| int {SQL_VARBINARY} |] ==) -> True) where
  SQL_VARBINARY = SQLType [C.pure| int {SQL_VARBINARY} |]

pattern SQL_LONGVARBINARY :: SQLType
pattern SQL_LONGVARBINARY <- ((SQLType [C.pure| int {SQL_LONGVARBINARY} |] ==) -> True) where
  SQL_LONGVARBINARY = SQLType [C.pure| int {SQL_LONGVARBINARY} |]

pattern SQL_BIGINT :: SQLType
pattern SQL_BIGINT <- ((SQLType [C.pure| int {SQL_BIGINT} |] ==) -> True) where
  SQL_BIGINT = SQLType [C.pure| int {SQL_BIGINT} |]

pattern SQL_TINYINT :: SQLType
pattern SQL_TINYINT <- ((SQLType [C.pure| int {SQL_TINYINT} |] ==) -> True) where
  SQL_TINYINT = SQLType [C.pure| int {SQL_TINYINT} |]

pattern SQL_BIT :: SQLType
pattern SQL_BIT <- ((SQLType [C.pure| int {SQL_BIT} |] ==) -> True) where
  SQL_BIT = SQLType [C.pure| int {SQL_BIT} |]

pattern SQL_GUID :: SQLType
pattern SQL_GUID <- ((SQLType [C.pure| int {SQL_GUID} |] ==) -> True) where
  SQL_GUID = SQLType [C.pure| int {SQL_GUID} |]

pattern SQL_WCHAR :: SQLType
pattern SQL_WCHAR <- ((SQLType [C.pure| int {SQL_WCHAR} |] ==) -> True) where
  SQL_WCHAR = SQLType [C.pure| int {SQL_WCHAR} |]

pattern SQL_WVARCHAR :: SQLType
pattern SQL_WVARCHAR <- ((SQLType [C.pure| int {SQL_WVARCHAR} |] ==) -> True) where
  SQL_WVARCHAR = SQLType [C.pure| int {SQL_WVARCHAR} |]

pattern SQL_WLONGVARCHAR :: SQLType
pattern SQL_WLONGVARCHAR <- ((SQLType [C.pure| int {SQL_WLONGVARCHAR} |] ==) -> True) where
  SQL_WLONGVARCHAR = SQLType [C.pure| int {SQL_WLONGVARCHAR} |]  

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
 :: SQLType
 #-}

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

{-# COMPLETE
   SQL_HANDLE_ENV
 , SQL_HANDLE_DBC
 , SQL_HANDLE_STMT
 , SQL_HANDLE_DESC
 :: HandleType
 #-}


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

-} 
