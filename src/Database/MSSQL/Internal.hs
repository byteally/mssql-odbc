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
{-# LANGUAGE DeriveGeneric              #-}

module Database.MSSQL.Internal
  ( module Database.MSSQL.Internal.SQLError
  , module Database.MSSQL.Internal.ConnectAttribute
  , module Database.MSSQL.Internal.Types
  , module Database.MSSQL.Internal.SQLBindCol
  , module Database.MSSQL.Internal
  ) where

import Database.MSSQL.Internal.ConnectAttribute
import Database.MSSQL.Internal.Types

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BSB
import qualified Language.C.Inline as C
import Foreign.Storable
import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc (alloca)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Coerce
import Database.MSSQL.Internal.Ctx
import Database.MSSQL.Internal.SQLError
import Data.Text.Foreign as T
import qualified Data.Text as T
import Data.IORef
import Data.Word
import Data.Int
import GHC.Generics
import Data.Time
import Data.UUID.Types (UUID)
import Database.MSSQL.Internal.SQLTypes
import Data.Functor.Identity
import Control.Monad
import Data.String
#if __GLASGOW_HASKELL__ < 802
import Data.Semigroup
#endif
import Data.Scientific
import Data.Typeable
import GHC.TypeLits
import Data.Char (isAscii)
import Control.Exception (bracket, onException, finally, throwIO)
import qualified Foreign.C.String as F
import Database.MSSQL.Internal.SQLBindCol
import qualified Data.Text.Encoding as TE

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

connectInfo :: ConnectionString -> ConnectInfo
connectInfo conStr = ConnectInfo
  { connectionString = conStr
  , attrBefore = mempty
  , attrAfter = mempty
  }

connect :: ConnectInfo -> IO Connection
connect connInfo = do
  alloca $ \(henvp :: Ptr (Ptr SQLHENV)) -> do
    alloca $ \(hdbcp :: Ptr (Ptr SQLHDBC)) -> do
      doConnect henvp hdbcp
  where
    setAttrsBeforeConnect :: Ptr SQLHDBC -> ConnectAttr 'ConnectBefore -> IO ()
    setAttrsBeforeConnect = setConnectAttr

    setAttrsAfterConnect :: Ptr SQLHDBC -> ConnectAttr 'ConnectAfter -> IO ()
    setAttrsAfterConnect  = setConnectAttr

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

        return ret;
        }|]

      henv <- peek henvp
      hdbc <- peek hdbcp

      when (not (isSuccessful ret)) $
        getErrors ret (SQLDBCRef hdbc) >>= throwSQLException
      mapM_ (setAttrsBeforeConnect hdbc) (attrBefore connInfo)

      ret' <- ResIndicator <$> [C.block| int {
        SQLRETURN ret = 0;
        SQLHENV* henvp = $(SQLHENV* henvp);
        SQLHDBC* hdbcp = $(SQLHDBC* hdbcp);
      
        SQLWCHAR* cstr = $fptr-ptr:(SQLWCHAR * ctxt);
        ret = SQLDriverConnectW(*hdbcp, 0, cstr, (SQLSMALLINT)$(int ctxtLen), 0, 0, 0, SQL_DRIVER_NOPROMPT);

        return ret;
        }|]

      mapM_ (setAttrsAfterConnect hdbc) (attrAfter connInfo)
      case isSuccessful ret' of
        False -> getErrors ret' (SQLDBCRef hdbc) >>= throwSQLException
        True -> pure $ Connection { _henv = henv, _hdbc = hdbc }

disconnect :: Connection -> IO ()
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
    True -> pure ()
    False -> do
      dbErrs <- getErrors ret (SQLDBCRef hdbc)
      envErrs <- getErrors ret (SQLENVRef henv)
      throwSQLException (dbErrs <> envErrs)
  where
    hdbc = _hdbc con
    henv = _henv con

sqlExecDirect :: Ptr SQLHSTMT -> T.Text -> IO ResIndicator
sqlExecDirect hstmtP sql = do
  (queryWStr, queryLen) <- fmap (fmap fromIntegral) $ asForeignPtr $ sql

  ResIndicator <$> [C.block| int {
    SQLRETURN ret = 0;
    SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
    ret = SQLExecDirectW(hstmt, $fptr-ptr:(SQLWCHAR* queryWStr), $(int queryLen));
    return ret;
    }|]
  
        
allocHSTMT :: Connection -> IO (HSTMT a)
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
      False -> getErrors ret (SQLDBCRef hdbc) >>= throwSQLException
      True -> do
        hstmt <- peek hstmtp
        cpos <- initColPos
        pure $ HSTMT hstmt cpos 0
  where
    hdbc = _hdbc con


releaseHSTMT :: HSTMT a -> IO ()
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
    True -> pure ()
    False -> getErrors ret (SQLSTMTRef hstmt) >>= throwSQLException
  where
    hstmt = getHSTMT stmt

withHSTMT :: Connection -> (HSTMT a -> IO a) -> IO a
withHSTMT con act = do
  bracket (allocHSTMT con)
          releaseHSTMT
          act

sqlNumResultCols :: Ptr CShort -> Ptr SQLHSTMT-> IO ResIndicator
sqlNumResultCols numResultColsP hstmt = do
  ResIndicator <$> [C.block| int {
    SQLRETURN ret = 0;
    SQLHSTMT hstmt = $(SQLHSTMT hstmt);
    SQLSMALLINT* numColumnPtr = $(SQLSMALLINT* numResultColsP);
    ret = SQLNumResultCols(hstmt, numColumnPtr);
    return ret;
    }|]

sqlFetch :: HSTMT a -> IO ResIndicator
sqlFetch stmt = do
  ResIndicator <$>
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

sqlMoreResults :: Ptr SQLHSTMT -> IO ResIndicator
sqlMoreResults hstmt = 
    ResIndicator <$> [C.block| SQLRETURN {
                        SQLRETURN ret = 0;  
                        SQLHSTMT hstmt = $(SQLHSTMT hstmt);
                        ret = SQLMoreResults(hstmt);
                        return ret;
                     }|]

withTransaction :: Connection -> IO a -> IO a
withTransaction (Connection { _hdbc = hdbcp }) io = do
  setAutoCommitOff hdbcp
  go `onException` rollback hdbcp
     `finally` setAutoCommitOn hdbcp
  where go = do
          a <- io
          commit hdbcp
          pure a

setConnectAttr :: Ptr SQLHDBC -> ConnectAttr a -> IO ()
setConnectAttr hdbcp connAttr = do
  (attr, fvptr, len) <- setConnectAttrPtr connAttr
  ret <- withForeignPtr fvptr $ \vptr -> sqlSetConnectAttr hdbcp attr vptr len
  case isSuccessful ret of
    False -> do
      getErrors ret (SQLDBCRef hdbcp) >>= throwSQLException
    True -> pure ()

{-
getConnectAttr :: Ptr SQLHDBC -> AttrName -> IO (Maybe (ConnectAttr a))
getConnectAttr hdbcp attr = do
  let clen = case isFixedSizeAttrValue attr of
               -- NOTE: Fixed length types are either SQLULEN or
               --       SQLUINTEGER. hence going with SQL_IS_UINTEGER.
               True -> [C.pure| SQLINTEGER { SQL_IS_UINTEGER } |]

               -- NOTE: Making a choice here. If this is not acceptable
               --       call sqlGenConnectAttr manually.
               False -> 100
  vptr <- mallocForeignPtrBytes (fromIntegral clen)
  outLenPtr <- mallocForeignPtr
  -- TODO: Something about setting outLenPtr to 0 in doc.
  ret <- sqlGetConnectAttr hdbcp (getAttrName attr) vptr clen outLenPtr
  case isSuccessful ret of
    False -> do
      getErrors ret (SQLDBCRef hdbcp) >>= throwSQLException
    True -> getConnectAttrPtr attr (castForeignPtr vptr) outLenPtr
-}

setAutoCommitOn :: Ptr SQLHDBC -> IO ()
setAutoCommitOn hdbcp = do
  ret <- sqlSetConnectAttr hdbcp [C.pure| SQLINTEGER { SQL_ATTR_AUTOCOMMIT } |]
                          (intPtrToPtr (fromIntegral [C.pure| SQLUINTEGER { SQL_AUTOCOMMIT_ON }|]))
                          [C.pure| SQLINTEGER { SQL_IS_UINTEGER } |]  
  case isSuccessful ret of
    False -> do
      getErrors ret (SQLDBCRef hdbcp) >>= throwSQLException
    True -> pure ()

setAutoCommitOff :: Ptr SQLHDBC -> IO ()
setAutoCommitOff hdbcp = do
  ret <- sqlSetConnectAttr hdbcp [C.pure| SQLINTEGER { SQL_ATTR_AUTOCOMMIT } |]
                          (intPtrToPtr (fromIntegral [C.pure| SQLUINTEGER { SQL_AUTOCOMMIT_OFF }|]))
                          [C.pure| SQLINTEGER { SQL_IS_UINTEGER } |]
  case isSuccessful ret of
    False -> do
      getErrors ret (SQLDBCRef hdbcp) >>= throwSQLException
    True -> pure ()

commit :: Ptr SQLHDBC -> IO ()
commit hdbcp = do
  ret <- sqlEndTran hdbcp [C.pure| SQLSMALLINT { SQL_COMMIT } |]
  case isSuccessful ret of
    False -> do
      getErrors ret (SQLDBCRef hdbcp) >>= throwSQLException
    True -> pure ()

rollback :: Ptr SQLHDBC -> IO ()
rollback hdbcp = do
  ret <- sqlEndTran hdbcp [C.pure| SQLSMALLINT { SQL_ROLLBACK } |]
  case isSuccessful ret of
    False -> do
      getErrors ret (SQLDBCRef hdbcp) >>= throwSQLException
    True -> pure ()

sqlEndTran :: Ptr SQLHDBC -> CShort -> IO ResIndicator
sqlEndTran hdbcp completionType = do
  ResIndicator <$> [C.block| int {
      SQLRETURN ret = 0;
      SQLHDBC hdbcp = $(SQLHDBC hdbcp);
      SQLSMALLINT completion_type = $(SQLSMALLINT completionType);
      ret = SQLEndTran(SQL_HANDLE_DBC, hdbcp, completion_type);
      return ret;
      } |]

sqlSetConnectAttr :: Ptr SQLHDBC -> CLong -> Ptr () -> CLong -> IO ResIndicator
sqlSetConnectAttr hdbcp attr vptr len = 
  ResIndicator <$> [C.block| int {
      SQLRETURN ret = 0;
      SQLHDBC hdbcp = $(SQLHDBC hdbcp);
      SQLINTEGER attr = $(SQLINTEGER attr);
      SQLPOINTER vptr =  $(SQLPOINTER vptr); 
      SQLINTEGER len = $(SQLINTEGER len);      
      ret = SQLSetConnectAttr(hdbcp, attr, vptr, len);
      return ret;
      } |]

sqlGetConnectAttr :: Ptr SQLHDBC -> CLong -> ForeignPtr () -> CLong -> ForeignPtr CLong -> IO ResIndicator
sqlGetConnectAttr hdbcp attr vptr len outLenPtr =
  ResIndicator <$> [C.block| int {
      SQLRETURN ret = 0;
      SQLHDBC hdbcp = $(SQLHDBC hdbcp);
      SQLINTEGER attr = $(SQLINTEGER attr);
      SQLPOINTER vptr = $fptr-ptr:(SQLPOINTER vptr);
      SQLINTEGER len = $(SQLINTEGER len);
      SQLINTEGER* outLenPtr = $fptr-ptr:(SQLINTEGER* outLenPtr);            
      ret = SQLGetConnectAttr(hdbcp, attr, vptr, len, outLenPtr);
      return ret;
      } |]

  
extractVal :: Storable t => ColBufferType 'BindCol t -> IO t
extractVal = extractWith (\_ -> peek)

extractWith ::
  Storable t =>
  (CLong -> Ptr t -> IO b) ->  
  ColBufferType 'BindCol t ->
  IO b
extractWith f cbuff = case cbuff of
  BindColBuffer cdesc lenOrIndFP cbuffPtr -> do
    lenOrInd <- peekFP lenOrIndFP
    case fromIntegral lenOrInd of
      SQL_NULL_DATA -> throwIO (SQLNullDataException cdesc)
      SQL_NO_TOTAL -> throwIO (SQLNoTotalException cdesc)
      _ -> withForeignPtr cbuffPtr (f lenOrInd)

castColBufferPtr ::
  (Coercible t1 t2) =>
  ColBufferType bt t1 ->
  ColBufferType bt t2
castColBufferPtr cbuff = case cbuff of
  BindColBuffer cdesc lenOrIndFP cbuffPtr ->
    BindColBuffer cdesc lenOrIndFP (castForeignPtr cbuffPtr)
  GetDataUnboundBuffer cdesc bufSize k ->
    GetDataUnboundBuffer cdesc bufSize (coerce k)
  GetDataBoundBuffer cdesc act ->
    GetDataBoundBuffer cdesc $ do
      (fptr, lenOrInd) <- act
      pure (castForeignPtr fptr, lenOrInd)

boundWith ::
  ColBufferType 'GetDataBound t ->
  (CLong -> Ptr t -> IO a)       ->
  IO a
boundWith (GetDataBoundBuffer cdesc io) f = do
  (fptr, lenOrIndFP) <- io
  withForeignPtr fptr $ \ptr ->
    withForeignPtr lenOrIndFP $ \lenOrIndP -> do
      lenOrInd <- peek lenOrIndP
      case fromIntegral lenOrInd of
        SQL_NULL_DATA -> throwIO (SQLNullDataException cdesc)
        SQL_NO_TOTAL -> throwIO (SQLNoTotalException cdesc)
        _ -> f lenOrInd ptr

newtype ASCIIText = ASCIIText { getASCIIText :: T.Text }
                  deriving (Show, Eq, Generic)

instance (KnownNat n) => IsString (Sized n T.Text) where
  fromString = Sized . T.pack . lengthCheck
    where lengthCheck x = case length x > fromIntegral n of
            True -> error "Panic: string size greater than sized parameter"
            False -> x

          n = natVal (Proxy :: Proxy n)

instance (KnownNat n) => IsString (Sized n ASCIIText) where
  fromString = Sized . ASCIIText . lengthCheck . getASCIIText . fromString
    where n = natVal (Proxy :: Proxy n)
          lengthCheck x = case T.length x > fromIntegral n of
            True -> error "Panic: string size greater than sized parameter"
            False -> x

instance IsString ASCIIText where
  fromString = ASCIIText . T.pack . checkAscii
    where checkAscii = map (\a -> case isAscii a of
                               True -> a
                               False -> error $ "Panic: non ascii character in ASCIIText " ++ show a)
  
newtype Sized (size :: Nat) a = Sized { getSized :: a }
                              deriving (Generic, Show, Eq, Ord)
  
fetchRows :: HSTMT a -> IO r -> IO (Vector r, ResIndicator)
fetchRows hstmt rowP = do
  retRef <- newIORef SQL_SUCCESS
  rows <- flip V.unfoldrM () $ \_ -> do
    res <- sqlFetch hstmt
    case res of
      SQL_SUCCESS -> do
        r <- rowP
        pure $ Just (r, ())
    
      ret -> atomicModifyIORef' retRef (\r -> (ret, r)) *> pure Nothing
  ret <- readIORef retRef
  pure (rows, ret)

sqlRowCount :: HSTMT a -> IO Int64
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
    True -> fromIntegral <$> peekFP rcountFP
    False -> do
      errs <- getErrors ret (SQLSTMTRef hstmt)
      throwSQLException errs
  where
    hstmt = getHSTMT stmt


data FieldDescriptor t = FieldDescriptor

initColPos :: IO ColPos
initColPos = ColPos <$> newIORef 1

reinitColPos :: ColPos -> IO ()
reinitColPos (ColPos ref) =
  atomicWriteIORef ref 1

nextColPos :: HSTMT a -> IO ()
nextColPos hstmt = do
  let (ColPos wref) = colPos hstmt
  atomicModifyIORef' wref (\w -> (w +1, ()))

getColPos :: HSTMT a -> IO CUShort
getColPos hstmt = do
  let (ColPos wref) = colPos hstmt
  readIORef wref

type Query = T.Text

query :: forall r.(FromRow r) => Connection -> Query -> IO (Vector r)
query = queryWith defQueryConfig fromRow 

queryWith :: forall r.QueryConfig -> RowParser r -> Connection -> Query -> IO (Vector r)
queryWith cfg rowP con q = do
  exec con q go

  where go hstmt res = do
          case res of
            SQL_SUCCESS -> do
              fptrCols :: ForeignPtr CShort <- mallocForeignPtr                
              resNumResCols <- withForeignPtr fptrCols $ \ptrCols -> sqlNumResultCols ptrCols (getHSTMT hstmt)
              case isSuccessful resNumResCols of
                True -> do
                  nrcs <- peekFP fptrCols
                  fetch cfg rowP hstmt nrcs
                False -> do                
                  errs <- getErrors resNumResCols (SQLSTMTRef $ getHSTMT hstmt)
                  throwSQLException errs                    
            _ -> do
              errs <- getErrors res (SQLSTMTRef $ getHSTMT hstmt)
              throwSQLException errs                    

fetch :: QueryConfig -> RowParser a -> HSTMT (Vector a) -> CShort -> IO (Vector a)
fetch cfg (RowParser colBuf rowPFun) hstmt nrcs = do
   bt <- bindColumnStrategy cfg hstmt nrcs
   reinitColPos (colPos hstmt)
   colBuffer <- colBuf bt cfg ((coerce hstmt { numResultCols = nrcs }) :: HSTMT ())
   (rows, ret) <- fetchRows hstmt (rowPFun colBuffer)
   case ret of
         SQL_SUCCESS           -> pure rows
         SQL_SUCCESS_WITH_INFO -> pure rows
         SQL_NO_DATA           -> pure rows
         _                     -> do
           errs <- getErrors ret (SQLSTMTRef $ getHSTMT hstmt)
           throwSQLException errs

execute :: Connection -> Query -> IO Int64
execute con q =
  exec con q $ \hstmt res -> case res of
                               SQL_SUCCESS -> sqlRowCount hstmt
                               SQL_NO_DATA -> pure 0
                               _           -> getErrors res (SQLSTMTRef (getHSTMT hstmt)) >>= throwSQLException

exec :: Connection -> Query -> (HSTMT a -> ResIndicator -> IO a) -> IO a
exec con q f = do
  withHSTMT con $ \hstmt -> do
    res <- sqlExecDirect (getHSTMT hstmt) q
    f hstmt res
  
data RowParser t =
  forall rowbuff.
  RowParser { rowBuffer    :: BindType -> QueryConfig -> HSTMT () -> IO rowbuff
            , runRowParser :: rowbuff -> IO t
            }

instance Functor RowParser where
  fmap f (RowParser b rpf) = RowParser b $ \b' -> do
    res <- rpf b'
    pure (f res)

instance Applicative RowParser where
  pure a = RowParser (\_ _ _ -> pure ()) (const (pure a))
  (RowParser b1 f) <*> (RowParser b2 a) =
    RowParser (\bt cfg hstmt -> (,) <$> b1 bt cfg hstmt <*> b2 bt cfg hstmt) $
    \(b1', b2') -> f b1' <*> a b2'

field :: forall f. FromField f => RowParser f
field = RowParser
        (\bt cfg s ->
            bindColumn bt cfg (restmt s :: HSTMT (FieldBufferType f))
        )
        fromField

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

class ( SQLBindCol (FieldBufferType t)
      ) => FromField t where
  type FieldBufferType t :: *
  fromField :: ColBuffer (FieldBufferType t) -> IO t

instance FromField Int where
  type FieldBufferType Int = CBigInt
  fromField =
    either (\v -> extractVal v >>= pure . fromIntegral)
           (\v -> boundWith v (\_ -> fmap fromIntegral . peek))
    .
    getColBuffer
  
{-
instance FromField Int8 where
  type FieldBufferType Int8 = CTinyInt
  fromField = Value $ \i -> extractVal i >>= (\v -> pure $ fromIntegral v)
-}
  
instance FromField Int16 where
  type FieldBufferType Int16 = CSmallInt
  fromField =
    either (\v -> extractVal v >>= pure . fromIntegral)
           (\v -> boundWith v (\_ -> fmap fromIntegral . peek))
    .
    getColBuffer

instance FromField Int32 where
  type FieldBufferType Int32 = CLong
  fromField =
    either (\v -> extractVal v >>= pure . fromIntegral)
           (\v -> boundWith v (\_ -> fmap fromIntegral . peek))
    .
    getColBuffer

instance FromField Int64 where
  type FieldBufferType Int64 = CBigInt
  fromField =
    either (\v -> extractVal v >>= pure . fromIntegral)
           (\v -> boundWith v (\_ -> fmap fromIntegral . peek))
    .
    getColBuffer

instance FromField Word8 where
  type FieldBufferType Word8 = CUTinyInt
  fromField =
    either (\v -> extractVal v >>= pure . fromIntegral)
           (\v -> boundWith v (\_ -> fmap fromIntegral . peek))
    .
    getColBuffer

instance FromField Double where
  type FieldBufferType Double = CDouble
  fromField =
    either (\v -> extractVal v >>= pure . coerce)
           (\v -> boundWith v (\_ -> fmap coerce . peek))
    .
    getColBuffer

instance FromField Float where
  type FieldBufferType Float = CFloat
  fromField =
    either (\v -> extractVal v >>= pure . coerce)
           (\v -> boundWith v (\_ -> fmap coerce . peek))
    .
    getColBuffer

instance FromField Bool where
  type FieldBufferType Bool = CBool
  fromField =
    either (\v -> extractVal v >>= pure . boolFn)
           (\v -> boundWith v $ \_ a -> do
               v' <- peek a
               pure (boolFn v'))
    .
    getColBuffer

    where boolFn v = if v == 1 then True else False

instance (KnownNat n) => FromField (Sized n T.Text) where
  type FieldBufferType (Sized n T.Text) = CSized n (FieldBufferType T.Text)
  fromField = fmap Sized . fromField . coerce

instance (KnownNat n) => FromField (Sized n ASCIIText) where
  type FieldBufferType (Sized n ASCIIText) = CSized n (FieldBufferType ASCIIText)
  fromField = fmap Sized . fromField . coerce

instance (KnownNat n) => FromField (Sized n ByteString) where
  type FieldBufferType (Sized n ByteString) = CSized n (FieldBufferType ByteString)
  fromField = fmap Sized . fromField . coerce

instance FromField ASCIIText where
  type FieldBufferType ASCIIText = CChar
  fromField v = case getColBuffer v of
    Left v' -> case v' of
      BindColBuffer _ charCountFP textFP -> do
         charCount <- peekFP charCountFP
         a <- withForeignPtr textFP $ \ccharP -> F.peekCStringLen (coerce ccharP, fromIntegral charCount)
         pure (ASCIIText (T.pack a))
    Right v' -> case v' of
      GetDataUnboundBuffer cdesc _ io -> ASCIIText <$> getDataTxt cdesc io

    where getDataTxt cdesc io = do
            (fetchAct, ccharFP, lenOrIndFP) <- io
            go cdesc lenOrIndFP ccharFP fetchAct mempty

          go cdesc lenOrIndFP ccharFP fetchAct = loop0
            where loop0 acc = do
                      ret <- fetchAct
                      lenOrInd <- peekFP lenOrIndFP
                      case ret of
                        SQL_SUCCESS -> do
                          case fromIntegral lenOrInd of
                            SQL_NULL_DATA -> throwIO (SQLNullDataException cdesc)
                            _ -> do
                              a <- withForeignPtr ccharFP F.peekCString
                              pure (acc <> T.pack a)
                        SQL_SUCCESS_WITH_INFO -> do
                          case fromIntegral lenOrInd of
                            SQL_NULL_DATA -> throwIO (SQLNullDataException cdesc)
                            _ -> do                          
                              a <- withForeignPtr ccharFP F.peekCString
                              loop0 (acc <> T.pack a)                         
                        SQL_NO_DATA -> pure acc
                        _ -> error "Panic: impossible case @fromField ASCIIText"
            
instance FromField ByteString where
  type FieldBufferType ByteString = CBinary
  fromField v = case getColBuffer v of
    Left v' -> case v' of
      (BindColBuffer _ byteCountFP bytesFP) -> do
         byteCount <- peekFP byteCountFP
         withForeignPtr bytesFP $ \bytesP -> (do
                                         bs <- BS.packCStringLen (coerce bytesP, fromIntegral byteCount)
                                         pure bs
                                            )
         
    Right v' -> case v' of
      GetDataUnboundBuffer cdesc bufSize io -> LBS.toStrict . BSB.toLazyByteString <$> bsParts cdesc bufSize io

instance FromField LBS.ByteString where
  type FieldBufferType LBS.ByteString = CBinary
  fromField v = case getColBuffer v of
    Left v' -> case v' of
      (BindColBuffer _ byteCountFP bytesFP) -> do
         byteCount <- peekFP byteCountFP
         withForeignPtr bytesFP $ \bytesP -> (LBS.fromStrict <$> (BS.packCStringLen (coerce bytesP, fromIntegral byteCount)))

    Right v' -> case v' of
      GetDataUnboundBuffer cdesc bufSize io -> BSB.toLazyByteString <$> bsParts cdesc bufSize io

instance FromField Image where
  type FieldBufferType Image = CBinary
  fromField = fmap Image . fromField

instance FromField T.Text where
  type FieldBufferType T.Text = CText
  fromField v = case getColBuffer v of
    Left v' -> case v' of
      (BindColBuffer _ byteCountFP bytesFP) -> fmap TE.decodeUtf16LE $ do
         byteCount <- peekFP byteCountFP
         s <- withForeignPtr bytesFP $ \bytesP -> ((BS.packCStringLen (coerce bytesP, fromIntegral byteCount)))
         pure s
    Right v' -> case v' of
      GetDataUnboundBuffer cdesc bufSize io -> TE.decodeUtf16LE . LBS.toStrict . BSB.toLazyByteString <$> bsParts cdesc bufSize io

bsParts :: ColDescriptor -> CLong -> IO (IO ResIndicator, ForeignPtr t, ForeignPtr CLong) -> IO BSB.Builder
bsParts cdesc bufSize io = do
  (fetchAct, binFP, lenOrIndFP) <- io
  go lenOrIndFP binFP fetchAct mempty

  where 
    go lenOrIndFP binFP fetchAct = loop0
      where loop0 acc = do
                ret <- fetchAct
                lenOrInd <- peekFP lenOrIndFP
                case ret of
                  SQL_SUCCESS -> do
                    case fromIntegral lenOrInd of
                      SQL_NULL_DATA -> throwIO (SQLNullDataException cdesc)
                      SQL_NO_TOTAL -> throwIO (SQLNoTotalException cdesc)
                      size -> do
                        a <- withForeignPtr binFP $ \binP -> BS.packCStringLen (coerce binP, fromIntegral size)
                        pure (acc <> BSB.byteString a)
                  SQL_SUCCESS_WITH_INFO -> do
                    case fromIntegral lenOrInd of
                      SQL_NULL_DATA -> throwIO (SQLNullDataException cdesc)
                      SQL_NO_TOTAL -> do
                        a <- withForeignPtr binFP $ \binP -> BS.packCStringLen (coerce binP, fromIntegral bufSize)
                        loop0 (acc <> BSB.byteString a)
                      size | size > fromIntegral bufSize -> do
                        a <- withForeignPtr binFP $ \binP -> BS.packCStringLen (coerce binP, fromIntegral bufSize)
                        loop0 (acc <> BSB.byteString a)
                           | otherwise -> do
                        a <- withForeignPtr binFP $ \binP -> BS.packCStringLen (coerce binP, fromIntegral size)
                        loop0 (acc <> BSB.byteString a)                                     
                  SQL_NO_DATA -> pure acc
                  _ -> error "Panic: impossible case @bsParts"                  
  
instance FromField Day where
  type FieldBufferType Day = CDate
  fromField =
    either (\v -> extractVal v >>= (pure . getDate . coerce))
           (\v -> boundWith v (\_ -> fmap (getDate . coerce) . peek))
    .
    getColBuffer

instance FromField TimeOfDay where
  type FieldBufferType TimeOfDay = CTimeOfDay
  fromField =
    either (\v -> flip extractWith v $ \_ v' -> do
               v'' <- peek (coerce v')
               pure $ getTimeOfDay v''
           )
           (\v -> boundWith v (\_ -> fmap (getTimeOfDay . coerce) . peek))
    .
    getColBuffer

instance FromField LocalTime where
  type FieldBufferType LocalTime = CLocalTime
  fromField =
    either (\v -> extractVal v >>= (pure . getLocalTime . coerce))
           (\v -> boundWith v (\_ -> fmap (getLocalTime . coerce) . peek))
    .
    getColBuffer

instance FromField ZonedTime where
  type FieldBufferType ZonedTime = CZonedTime
  fromField =
    either (\v -> extractVal v >>= (pure . Database.MSSQL.Internal.SQLTypes.getZonedTime . coerce))
           (\v -> boundWith v (\_ -> fmap (Database.MSSQL.Internal.SQLTypes.getZonedTime . coerce) . peek))
    .
    getColBuffer

instance FromField UTCTime where
  type FieldBufferType UTCTime = FieldBufferType ZonedTime
  fromField = fmap zonedTimeToUTC . fromField

instance FromField UUID where
  type FieldBufferType UUID = UUID
  fromField =
    either (extractVal . castColBufferPtr)
           (\v -> boundWith v (\_ -> fmap coerce . peek))
    .
    getColBuffer

-- NOTE: There is no generic lengthOrIndicatorFP
instance (FromField a, Storable (FieldBufferType a), Typeable a) => FromField (Maybe a) where
  type FieldBufferType (Maybe a) = FieldBufferType a
  fromField v = case getColBuffer v of
      Left (BindColBuffer _ fptr _) -> do 
        lengthOrIndicator <- peekFP fptr
        if lengthOrIndicator == fromIntegral SQL_NULL_DATA -- TODO: Only long worked not SQLINTEGER
          then pure Nothing
          else Just <$> (fromField v)
      Right (GetDataBoundBuffer cdesc io) -> do 
        (fp, lenOrIndFP) <- io
        lenOrInd <- peekFP lenOrIndFP
        case lenOrInd == fromIntegral SQL_NULL_DATA of
          True  -> pure Nothing
          False -> Just <$> fromField (ColBuffer (Right (GetDataBoundBuffer cdesc (pure (castForeignPtr fp, lenOrIndFP)))))
      Right (GetDataUnboundBuffer cdesc bufSize io) -> do
        (next, fptr, lenOrIndFP) <- io
        res <- next
        lenOrInd <- peekFP lenOrIndFP
        case lenOrInd == fromIntegral SQL_NULL_DATA of
          False -> Just <$> fromField (ColBuffer (Right (GetDataUnboundBuffer cdesc bufSize (go res next fptr lenOrIndFP))))
          True -> pure Nothing

          where go res next fptr lenOrIndFP = do
                  ref <- newIORef False
                  pure (next' ref, fptr, lenOrIndFP)

                  where next' ref = do
                          b <- readIORef ref
                          if b then next else atomicWriteIORef ref True >> pure res
            
      _ -> error "Panic: impossible case @fromField"
  
instance FromField a => FromField (Identity a) where
  type FieldBufferType (Identity a) = FieldBufferType a
  fromField = \v ->
      Identity <$> (fromField v)

inQuotes :: BSB.Builder -> BSB.Builder
inQuotes b = quote `mappend` b `mappend` quote
  where quote = BSB.char8 '\''  

#if  !MIN_VERSION_GLASGOW_HASKELL(8,2,0,0)
newtype CBool = CBool Word8
  deriving (Num, Eq, Storable)
#endif

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

bindColumnStrategy :: QueryConfig -> HSTMT a -> CShort -> IO BindType
bindColumnStrategy cfg hstmt =
  fmap go . mapM (sqlDescribeCol (getHSTMT hstmt) . fromIntegral) . enumFromTo 1

  where go = foldr accFn SQLBind
        maxColSize = boundSizeLimit cfg 
        accFn desc acc =
          case desc of
             d | colDataType d `elem` unboundedTypes -> SQLGetData
               | colSize d > maxColSize  -> SQLGetData
               | otherwise -> acc

          where unboundedTypes = [ SQL_WLONGVARCHAR, SQL_LONGVARCHAR, SQL_LONGVARBINARY ]


queryMany :: (ResultSets r) => Connection -> Query -> IO r
queryMany con q = do
  fptrCols :: ForeignPtr CShort <- mallocForeignPtr  
  exec con q $ \hstmt res -> resultSets hstmt res fptrCols

class ResultSets a where
  resultSets :: HSTMT a -> ResIndicator -> ForeignPtr CShort -> IO a

instance ResultSets () where
  resultSets _ _ _ = pure ()

instance (FromRow a, Typeable a) => ResultSets (Identity (Vector a)) where
  resultSets hstmt res fptrCols = do
    (xs, _) <- resultSets (coerce hstmt :: HSTMT (Vector a, ())) res fptrCols
    pure (Identity xs)

instance (ResultSets b, FromRow a, Typeable a) => ResultSets (Vector a, b) where
  resultSets =
    go

    where go hstmt res fptrCols =
            case res == SQL_SUCCESS || res == SQL_NO_DATA of
              False -> throwIO SQLResultSetException
              True -> do
                resNumResCols <- withForeignPtr fptrCols $ \ptrCol -> sqlNumResultCols ptrCol (getHSTMT hstmt)
                case isSuccessful resNumResCols of
                  True -> do
                    colCount <- peekFP fptrCols
                    case colCount > 0 of
                      True -> do
                        putStrLn $ "ColCount: " ++ show colCount ++ ", " ++ show (typeOf (undefined :: a))
                        vs <- fetch defQueryConfig fromRow (coerce hstmt :: HSTMT (Vector a)) colCount
                        nextRes <- sqlMoreResults (getHSTMT hstmt)
                        putStrLn "Recursing"
                        resSet <- resultSets (coerce hstmt :: HSTMT b) nextRes fptrCols
                        pure (vs, resSet)
                      False -> do
                        nextRes <- sqlMoreResults (getHSTMT hstmt)
                        go hstmt nextRes fptrCols
                  False -> do
                    errs <- getErrors resNumResCols (SQLSTMTRef $ getHSTMT hstmt)
                    throwSQLException errs                    
  
