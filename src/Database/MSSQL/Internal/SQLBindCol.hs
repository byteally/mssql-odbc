{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeFamilies               #-}

module Database.MSSQL.Internal.SQLBindCol
       ( CUTinyInt (..)
       , CTinyInt (..)
       , CSmallInt (..)
       , CUSmallInt (..)
       , CBigInt (..)
       , CUBigInt (..)
       , CSized (..)
       , CBinary (..)
       , CText (..)
       , BindType (..)

       , ColBuffer (..)
       , SQLBindCol (..)

       , bindColumn
       , peekFP
       , getErrors
       , isSuccessful
       , sqlDescribeCol
       ) where

import Foreign.C
import Foreign.Storable
import Database.MSSQL.Internal.Types
import Data.UUID.Types (UUID)
import Foreign.ForeignPtr
import qualified Language.C.Inline as C
import Foreign.Ptr
import Database.MSSQL.Internal.SQLTypes
import Database.MSSQL.Internal.Ctx
import Data.Typeable
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Database.MSSQL.Internal.SQLError
import Data.Coerce
import Data.IORef
import Control.Monad
import Control.Exception (throwIO)
import Data.Word
import Data.Text.Foreign as T
import GHC.TypeLits

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

newtype CUTinyInt = CUTinyInt CUChar
                  deriving (Show, Eq, Ord, Enum, Bounded, Num, Integral, Real, Storable)

newtype CTinyInt = CTinyInt CChar
                  deriving (Show, Eq, Ord, Enum, Bounded, Num, Integral, Real, Storable)

newtype CSmallInt = CSmallInt CShort
                  deriving (Show, Eq, Ord, Enum, Bounded, Num, Integral, Real, Storable)

newtype CUSmallInt = CUSmallInt CShort
                  deriving (Show, Eq, Ord, Enum, Bounded, Num, Integral, Real, Storable)

newtype CBigInt = CBigInt CLLong
                  deriving (Show, Eq, Ord, Enum, Bounded, Num, Integral, Real, Storable)

newtype CUBigInt = CUBigInt CULLong
                  deriving (Show, Eq, Ord, Enum, Bounded, Num, Integral, Real, Storable)

data BindType =
    SQLBind
  | SQLGetData

bindColumn :: (SQLBindCol t) => BindType -> QueryConfig -> HSTMT t -> IO (ColBuffer t)
bindColumn SQLBind _ = fmap (ColBuffer . Left) . sqlBindCol
bindColumn SQLGetData cfg = fmap (ColBuffer . Right) . sqlGetData cfg

newtype ColBuffer t = ColBuffer
  { getColBuffer :: Either (ColBufferType 'BindCol t)
                          (ColBufferType (GetColBufferTypeFromSize (ColBufferSize t)) t)
  } 

class SQLBindCol t where
  type ColBufferSize t :: ColBufferSizeK
  type ColBufferSize t = 'Bounded
  
  sqlBindCol :: HSTMT t -> IO (ColBufferType 'BindCol t)
  sqlGetData :: QueryConfig -> HSTMT t -> IO (ColBufferType (GetColBufferTypeFromSize (ColBufferSize t)) t)

sqlBindColTpl :: forall t. (Typeable t) =>
                      HSTMT t ->
                      (Ptr SQLHSTMT -> ColDescriptor -> IO (ColBufferType 'BindCol t)) ->
                      IO (ColBufferType 'BindCol t)
sqlBindColTpl hstmt block = do
   let hstmtP = getHSTMT hstmt       
   cdesc <- getCurrentColDescriptorAndMove hstmt
   if match cdesc
     then block hstmtP cdesc
     else typeMismatch exps cdesc

     where match cdesc = colDataType cdesc `elem` exps
           exps        = maybe [] id (HM.lookup rep sqlMapping)
           rep         = typeOf (undefined :: t)

sqlGetDataTplBound :: forall t. (Typeable t) =>
                      HSTMT t ->
                      (Ptr SQLHSTMT -> ColDescriptor -> IO (ColBufferType 'GetDataBound t)) ->
                      IO (ColBufferType 'GetDataBound t)
sqlGetDataTplBound hstmt block = do
   let hstmtP = getHSTMT hstmt       
   cdesc <- getCurrentColDescriptorAndMove hstmt
   if match cdesc
     then block hstmtP cdesc
     else typeMismatch exps cdesc

     where match cdesc = colDataType cdesc `elem` exps
           exps        = maybe [] id (HM.lookup rep sqlMapping)
           rep         = typeOf (undefined :: t)

sqlGetDataTplUnbound :: forall t. (Typeable t) =>
                      HSTMT t ->
                      (Ptr SQLHSTMT -> ColDescriptor -> IO (ColBufferType 'GetDataUnbound t)) ->
                      IO (ColBufferType 'GetDataUnbound t)
sqlGetDataTplUnbound hstmt block = do
   let hstmtP = getHSTMT hstmt       
   cdesc <- getCurrentColDescriptorAndMove hstmt
   if match cdesc
     then block hstmtP cdesc
     else typeMismatch exps cdesc

     where match cdesc = colDataType cdesc `elem` exps
           exps        = maybe [] id (HM.lookup rep sqlMapping)
           rep         = typeOf (undefined :: t)

newtype CSized (n :: Nat) a = CSized { getCSized :: a }
                deriving (Show, Eq, Ord, Enum, Bounded, Num, Integral, Real, Storable)

newtype CBinary = CBinary { getCBinary :: CUChar }
                deriving (Show, Eq, Ord, Enum, Bounded, Num, Integral, Real, Storable)

instance (KnownNat n, SQLBindCol a) => SQLBindCol (CSized n a) where
  type ColBufferSize (CSized n a) = ColBufferSize a
  sqlBindCol hstmt = do
    cdesc <- getCurrentColDescriptor hstmt
    let cSize = fromIntegral (colSize cdesc)
        tSize = fromIntegral (natVal (Proxy :: Proxy n))
    case cSize == tSize of
      False -> throwIO (SQLColumnSizeException tSize cSize)
      True -> coerce <$> sqlBindCol (restmt hstmt :: HSTMT a)
  sqlGetData cfg hstmt = do
    cdesc <- getCurrentColDescriptor hstmt
    let cSize = fromIntegral (colSize cdesc)
        tSize = fromIntegral (natVal (Proxy :: Proxy n))
    case cSize == tSize of
      False -> throwIO (SQLColumnSizeException tSize cSize)
      True -> coerce <$> sqlGetData cfg (restmt hstmt :: HSTMT a)

instance SQLBindCol CBinary where
  type ColBufferSize CBinary = 'Unbounded  
  sqlBindCol hstmt = do
    sqlBindColTpl hstmt $ \hstmtP cdesc -> do
      let
        cpos = colPosition cdesc
        bufSize = fromIntegral (colSize cdesc)
      binFP <- guardedMallocForeignPtrBytes (fromIntegral bufSize)
      lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
      ret <- fmap ResIndicator $ withForeignPtr binFP $ \binP -> do
        [C.block| int {
          SQLRETURN ret = 0;
          SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
          SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
          ret = SQLBindCol(hstmt, $(SQLUSMALLINT cpos), SQL_C_BINARY, $(SQLCHAR* binP), $(SQLLEN bufSize), lenOrInd);
          return ret;
        }|]
      withRetCode ret (SQLSTMTRef hstmtP) $
        bindColBuffer cdesc lenOrIndFP (coerce binFP)

  sqlGetData cfg hstmt = 
   sqlGetDataTplUnbound hstmt block
   
     where block hstmtP cdesc = do
             let bufSize = fromIntegral (bufferSize cfg)
             binFP <- guardedMallocForeignPtrBytes (fromIntegral bufSize)
             lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
             let cpos = colPosition cdesc
                 cbuf = getDataUnboundBuffer cdesc bufSize (fetchBytes hstmtP binFP lenOrIndFP bufSize cpos)
             pure cbuf

           fetchBytes hstmtP binFP lenOrIndFP bufSize cpos = pure (go, coerce binFP, lenOrIndFP)
             where go = do
                     ret <- fmap ResIndicator $ withForeignPtr binFP $ \binP -> do
                      [C.block| int {
                        SQLRETURN ret = 0;
                        SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
                        SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
                        ret = SQLGetData(hstmt, $(SQLUSMALLINT cpos), SQL_C_BINARY, $(SQLCHAR* binP), $(SQLLEN bufSize), lenOrInd);
                        return ret;
                      }|]
                     case ret `elem` [ SQL_SUCCESS, SQL_SUCCESS_WITH_INFO, SQL_NO_DATA ] of
                       True -> pure ret
                       False -> getErrors ret (SQLSTMTRef hstmtP) >>= throwSQLException

newtype CText = CText { getCText :: CBinary }
                deriving (Show, Eq, Ord, Enum, Bounded, Num, Integral, Real, Storable)

instance SQLBindCol CText where
  type ColBufferSize CText = 'Unbounded  
  sqlBindCol hstmt = do
    sqlBindColTpl hstmt $ \hstmtP cdesc -> do
      let
        cpos = colPosition cdesc
        bufSize = fromIntegral (colSize cdesc * 4 + 2)
      binFP <- guardedMallocForeignPtrBytes (fromIntegral bufSize)
      lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
      ret <- fmap ResIndicator $ withForeignPtr binFP $ \binP -> do
        [C.block| int {
          SQLRETURN ret = 0;
          SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
          SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
          ret = SQLBindCol(hstmt, $(SQLUSMALLINT cpos), SQL_C_BINARY, $(SQLCHAR* binP), $(SQLLEN bufSize), lenOrInd);
          return ret;
        }|]
      withRetCode ret (SQLSTMTRef hstmtP) $
        bindColBuffer cdesc lenOrIndFP (coerce binFP)

  sqlGetData cfg hstmt = 
   sqlGetDataTplUnbound hstmt block
   
     where block hstmtP cdesc = do
             let bufSize = fromIntegral (bufferSize cfg)
             binFP <- guardedMallocForeignPtrBytes (fromIntegral bufSize)
             lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
             let cpos = colPosition cdesc
                 cbuf = getDataUnboundBuffer cdesc bufSize (fetchBytes hstmtP binFP lenOrIndFP bufSize cpos)
             pure cbuf

           fetchBytes hstmtP binFP lenOrIndFP bufSize cpos = pure (go, coerce binFP, lenOrIndFP)
             where go = do
                     ret <- fmap ResIndicator $ withForeignPtr binFP $ \binP -> do
                      [C.block| int {
                        SQLRETURN ret = 0;
                        SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
                        SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
                        ret = SQLGetData(hstmt, $(SQLUSMALLINT cpos), SQL_C_BINARY, $(SQLCHAR* binP), $(SQLLEN bufSize), lenOrInd);
                        return ret;
                      }|]
                     case ret `elem` [ SQL_SUCCESS, SQL_SUCCESS_WITH_INFO, SQL_NO_DATA ] of
                       True -> pure ret
                       False -> getErrors ret (SQLSTMTRef hstmtP) >>= throwSQLException

instance SQLBindCol CChar where
  type ColBufferSize CChar = 'Unbounded  
  sqlBindCol hstmt =
    sqlBindColTpl hstmt $ \hstmtP cdesc -> do
      let
        cpos = colPosition cdesc
        bufSize = fromIntegral (colSize cdesc + 1)
      chrFP <- guardedMallocForeignPtrBytes (fromIntegral bufSize)
      lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
      ret <- fmap ResIndicator $ withForeignPtr chrFP $ \chrp -> do
        [C.block| int {
          SQLRETURN ret = 0;
          SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
          SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
          ret = SQLBindCol(hstmt, $(SQLUSMALLINT cpos), SQL_C_CHAR, $(SQLCHAR* chrp), $(SQLLEN bufSize), lenOrInd);
          return ret;
        }|]

      withRetCode ret (SQLSTMTRef hstmtP) $
        (bindColBuffer cdesc lenOrIndFP (coerce chrFP))
  
  sqlGetData cfg hstmt = 
   sqlGetDataTplUnbound hstmt block
   
     where block hstmtP cdesc = do
             let bufSize = fromIntegral (bufferSize cfg)
             chrFP <- guardedMallocForeignPtrBytes (fromIntegral bufSize)
             lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
             let cpos = colPosition cdesc
                 cbuf = getDataUnboundBuffer cdesc bufSize (fetchChars hstmtP chrFP lenOrIndFP bufSize cpos)
             pure cbuf

           fetchChars hstmtP chrFP lenOrIndFP bufSize cpos = pure (go, chrFP, lenOrIndFP)           
             where go = do
                     ret <- fmap ResIndicator $ withForeignPtr chrFP $ \chrP -> do
                      [C.block| int {
                        SQLRETURN ret = 0;
                        SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
                        SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
                        ret = SQLGetData(hstmt, $(SQLUSMALLINT cpos), SQL_C_CHAR, $(SQLCHAR* chrP), $(SQLLEN bufSize), lenOrInd);
                        return ret;
                      }|]
                     case ret `elem` [ SQL_SUCCESS, SQL_SUCCESS_WITH_INFO, SQL_NO_DATA ] of
                       True -> pure ret
                       False -> getErrors ret (SQLSTMTRef hstmtP) >>= throwSQLException
                      
instance SQLBindCol CUTinyInt where
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
        withRetCode ret (SQLSTMTRef hstmtP) $
          (bindColBuffer cdesc lenOrIndFP $ castForeignPtr chrFP)

  sqlGetData _ hstmt =  
   sqlGetDataTplBound hstmt $ \hstmtP cdesc -> pure $ getDataBoundBuffer cdesc $ do
        chrFP <- mallocForeignPtr
        let cpos = colPosition cdesc
        lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
        ret <- fmap ResIndicator $ withForeignPtr chrFP $ \chrP -> do
         [C.block| int {
             SQLRETURN ret = 0;
             SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
             SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
             ret = SQLGetData(hstmt, $(SQLUSMALLINT cpos), SQL_C_UTINYINT, $(SQLCHAR* chrP), sizeof(SQLCHAR), lenOrInd);
             return ret;
         }|]
        withRetCode ret (SQLSTMTRef hstmtP) $
          (coerce chrFP, lenOrIndFP)

instance SQLBindCol CLong where
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

      withRetCode ret (SQLSTMTRef hstmtP) $
        (bindColBuffer cdesc lenOrIndFP (coerce intFP))

  sqlGetData _ hstmt = 
   sqlGetDataTplBound hstmt $ \hstmtP cdesc -> pure $ getDataBoundBuffer cdesc $ do
      let cpos = colPosition cdesc
      intFP :: ForeignPtr CLong <- mallocForeignPtr
      lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
      ret <- fmap ResIndicator $ withForeignPtr intFP $ \intP -> do
       [C.block| int {
           SQLRETURN ret = 0;
           SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
           SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
           ret = SQLGetData(hstmt, $(SQLUSMALLINT cpos), SQL_C_LONG, $(SQLINTEGER* intP), sizeof(SQLINTEGER), lenOrInd);
           return ret;
       }|]

      withRetCode ret (SQLSTMTRef hstmtP) $
        (coerce intFP, lenOrIndFP)

instance SQLBindCol CSmallInt where
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

       withRetCode ret (SQLSTMTRef hstmtP) $
         (bindColBuffer cdesc lenOrIndFP $ coerce shortFP)

  sqlGetData _ hstmt = 
   sqlGetDataTplBound hstmt $ \hstmtP cdesc -> pure $ getDataBoundBuffer cdesc $ do 
       let cpos = colPosition cdesc
       shortFP <- mallocForeignPtr           
       lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
       ret <- fmap ResIndicator $ withForeignPtr shortFP $ \shortP -> do
        [C.block| int {
            SQLRETURN ret = 0;
            SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
            SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
            ret = SQLGetData(hstmt, $(SQLUSMALLINT cpos), SQL_C_SHORT, $(SQLSMALLINT* shortP), sizeof(SQLSMALLINT), lenOrInd);
            return ret;
        }|]

       withRetCode ret (SQLSTMTRef hstmtP) $
         (coerce shortFP, lenOrIndFP)

instance SQLBindCol CFloat where
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

      withRetCode ret (SQLSTMTRef hstmtP) $
        (bindColBuffer cdesc lenOrIndFP (coerce floatFP))

  sqlGetData _ hstmt =
   sqlGetDataTplBound hstmt $ \hstmtP cdesc -> pure $ getDataBoundBuffer cdesc $ do
      let cpos = colPosition cdesc
      floatFP <- mallocForeignPtr
      lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
      ret <- fmap ResIndicator $ withForeignPtr floatFP $ \floatP -> do
       [C.block| int {
           SQLRETURN ret = 0;
           SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
           SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
           ret = SQLGetData(hstmt, $(SQLUSMALLINT cpos), SQL_C_FLOAT, $(SQLREAL* floatP), sizeof(SQLREAL), lenOrInd);
           return ret;
       }|]

      withRetCode ret (SQLSTMTRef hstmtP) $
        (coerce floatFP, lenOrIndFP)


instance SQLBindCol CDouble where
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
      withRetCode ret (SQLSTMTRef hstmtP) $
        (bindColBuffer cdesc lenOrIndFP (coerce floatFP))

  sqlGetData _ hstmt =
   sqlGetDataTplBound hstmt $ \hstmtP cdesc -> pure $ getDataBoundBuffer cdesc $ do
      let cpos = colPosition cdesc
      floatFP <- mallocForeignPtr
      lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
      ret <- fmap ResIndicator $ withForeignPtr floatFP $ \floatP -> do
       [C.block| int {
           SQLRETURN ret = 0;
           SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
           SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
           ret = SQLGetData(hstmt, $(SQLUSMALLINT cpos), SQL_C_DOUBLE, $(SQLDOUBLE* floatP), sizeof(SQLDOUBLE), lenOrInd);
           return ret;
       }|]
      withRetCode ret (SQLSTMTRef hstmtP) $
        (coerce floatFP, lenOrIndFP)

instance SQLBindCol CBool where
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
      withRetCode ret (SQLSTMTRef hstmtP) $
        (bindColBuffer cdesc lenOrIndFP $ castForeignPtr chrFP)

  sqlGetData _ hstmt =
   sqlGetDataTplBound hstmt $ \hstmtP cdesc -> pure $ getDataBoundBuffer cdesc $ do    
      let cpos = colPosition cdesc
      chrFP <- mallocForeignPtr
      lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
      ret <- fmap ResIndicator $ withForeignPtr chrFP $ \chrP -> do
       [C.block| int {
           SQLRETURN ret = 0;
           SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
           SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
           ret = SQLGetData(hstmt, $(SQLUSMALLINT cpos), SQL_C_BIT, $(SQLCHAR* chrP), sizeof(1), lenOrInd);
           return ret;
       }|]
      withRetCode ret (SQLSTMTRef hstmtP) $
        (coerce chrFP, lenOrIndFP)

instance SQLBindCol CDate where
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

      withRetCode ret (SQLSTMTRef hstmtP) $
        (bindColBuffer cdesc lenOrIndFP (coerce dateFP))

  sqlGetData _ hstmt = do
   sqlGetDataTplBound hstmt $ \hstmtP cdesc -> pure $ getDataBoundBuffer cdesc $ do    
      let cpos = colPosition cdesc
      dateFP <- mallocForeignPtr
      lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
      ret <- fmap ResIndicator $ withForeignPtr dateFP $ \dateP -> do
       [C.block| int {
           SQLRETURN ret = 0;
           SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
           SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
           ret = SQLGetData(hstmt, $(SQLUSMALLINT cpos), SQL_C_TYPE_DATE, $(SQL_DATE_STRUCT* dateP), sizeof(SQL_DATE_STRUCT), lenOrInd);
           return ret;
       }|]

      withRetCode ret (SQLSTMTRef hstmtP) $
         (coerce dateFP, lenOrIndFP)

instance SQLBindCol CBigInt where
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

      withRetCode ret (SQLSTMTRef hstmtP) $
        (bindColBuffer cdesc lenOrIndFP $ coerce llongFP)

  sqlGetData _ hstmt =
      sqlGetDataTplBound hstmt $ \hstmtP cdesc -> pure $ getDataBoundBuffer cdesc $ do
       let cpos = colPosition cdesc
       llongFP :: ForeignPtr CLLong <- mallocForeignPtr
       lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
       ret <- fmap ResIndicator $ withForeignPtr llongFP $ \llongP -> do
        [C.block| int {
            SQLRETURN ret = 0;
            SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
            SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
            ret = SQLGetData(hstmt, $(SQLUSMALLINT cpos), SQL_C_SBIGINT, $(long long* llongP), sizeof(long long), lenOrInd);
            return ret;
        }|]

       withRetCode ret (SQLSTMTRef hstmtP) $
         (coerce llongFP, lenOrIndFP)

instance SQLBindCol CTimeOfDay where
  sqlBindCol hstmt = do
   sqlBindColTpl hstmt $ \hstmtP cdesc -> do    
     let cpos = colPosition cdesc
     todFP <- mallocForeignPtr 
     lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
     ret <- fmap ResIndicator $ withForeignPtr todFP $ \todP -> do
      [C.block| int {
          SQLRETURN ret = 0;
          SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
          SQLLEN bufSize = sizeof(SQL_SS_TIME2_STRUCT);
          SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
          ret = SQLBindCol(hstmt, $(SQLUSMALLINT cpos), SQL_C_BINARY, $(SQL_SS_TIME2_STRUCT* todP), sizeof(SQL_SS_TIME2_STRUCT), lenOrInd);
          return ret;
      }|]

     withRetCode ret (SQLSTMTRef hstmtP) $
       (bindColBuffer cdesc lenOrIndFP (coerce todFP))

  sqlGetData _ hstmt = do
   sqlGetDataTplBound hstmt $ \hstmtP cdesc -> pure $ getDataBoundBuffer cdesc $ do
     let cpos = colPosition cdesc
     todFP <- mallocForeignPtr 
     lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
     ret <- fmap ResIndicator $ withForeignPtr todFP $ \todP -> do
      [C.block| int {
          SQLRETURN ret = 0;
          SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
          SQLLEN bufSize = sizeof(SQL_SS_TIME2_STRUCT);
          SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
          ret = SQLGetData(hstmt, $(SQLUSMALLINT cpos), SQL_C_BINARY, $(SQL_SS_TIME2_STRUCT* todP), sizeof(SQL_SS_TIME2_STRUCT), lenOrInd);
          return ret;
      }|]

     withRetCode ret (SQLSTMTRef hstmtP) $
       (coerce todFP, lenOrIndFP)

instance SQLBindCol CLocalTime where
  sqlBindCol hstmt = do
   sqlBindColTpl hstmt $ \hstmtP cdesc -> do
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
       withRetCode ret (SQLSTMTRef hstmtP) $
         (bindColBuffer cdesc lenOrIndFP (coerce ltimeFP))

  sqlGetData _ hstmt = do
   sqlGetDataTplBound hstmt $ \hstmtP cdesc -> pure $ getDataBoundBuffer cdesc $ do
       let cpos = colPosition cdesc
       ltimeFP <- mallocForeignPtr
       lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
       ret <- fmap ResIndicator $ withForeignPtr ltimeFP $ \ltimeP -> do
        [C.block| int {
            SQLRETURN ret = 0;
            SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
            SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
            ret = SQLGetData(hstmt, $(SQLUSMALLINT cpos), SQL_C_TYPE_TIMESTAMP, $(SQL_TIMESTAMP_STRUCT* ltimeP), sizeof(SQL_TIMESTAMP_STRUCT), lenOrInd);
            return ret;
        }|]
       withRetCode ret (SQLSTMTRef hstmtP) $
         (coerce ltimeFP, lenOrIndFP)

instance SQLBindCol CZonedTime where
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

     withRetCode ret (SQLSTMTRef hstmtP) $
       (bindColBuffer cdesc lenOrIndFP (coerce ltimeFP))

  sqlGetData _ hstmt =
   sqlGetDataTplBound hstmt $ \hstmtP cdesc -> pure $ getDataBoundBuffer cdesc $ do    
     let cpos = colPosition cdesc
     ltimeFP <- mallocForeignPtr
     lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
     ret <- fmap ResIndicator $ withForeignPtr ltimeFP $ \ltimeP -> do
      [C.block| int {
          SQLRETURN ret = 0;
          SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
          SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
          ret = SQLGetData(hstmt, $(SQLUSMALLINT cpos), SQL_C_BINARY, $(SQL_SS_TIMESTAMPOFFSET_STRUCT* ltimeP), sizeof(SQL_SS_TIMESTAMPOFFSET_STRUCT), lenOrInd);
          return ret;
      }|]

     withRetCode ret (SQLSTMTRef hstmtP) $
       (coerce ltimeFP, lenOrIndFP)

instance SQLBindCol UUID where
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

     withRetCode ret (SQLSTMTRef hstmtP) $
       (bindColBuffer cdesc lenOrIndFP (coerce uuidFP))

  sqlGetData _ hstmt =
   sqlGetDataTplBound hstmt $ \hstmtP cdesc -> pure $ getDataBoundBuffer cdesc $ do    
     let cpos = colPosition cdesc
     uuidFP <- mallocForeignPtr
     lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
     ret <- fmap ResIndicator $ withForeignPtr uuidFP $ \uuidP -> do
      [C.block| int {
          SQLRETURN ret = 0;
          SQLHSTMT hstmt = $(SQLHSTMT hstmtP);
          SQLLEN* lenOrInd = $fptr-ptr:(SQLLEN* lenOrIndFP);
          ret = SQLGetData(hstmt, $(SQLUSMALLINT cpos), SQL_C_GUID, $(SQLGUID* uuidP), 16, lenOrInd);
          return ret;
      }|]

     withRetCode ret (SQLSTMTRef hstmtP) $
       (coerce uuidFP, lenOrIndFP)


typeMismatch :: [SQLType] -> ColDescriptor -> IO a
typeMismatch expTys col =
  let emsg = case expTys of
        [e] -> "Expected a type: " <> show e
        es  -> "Expected one of types: " <> show es
      msg = T.pack emsg <> ", but got a type: " <> T.pack (show colType) <> colMsg <> hintMsg
      hintMsg = "  HINT: " <> T.pack (show colType) <> " is mapped to the following " <> T.pack (show matches)
      colType = colDataType col
      matches = HM.foldlWithKey' (\a k v -> case colType `elem` v of
                                     True -> k : a
                                     False -> a
                                 ) [] sqlMapping
      colMsg = case T.unpack (colName col) of
        "" -> ""
        _  -> ", in a column: " <> colName col
  in  throwIO (SQLTypeMismatchException msg) 

getCurrentColDescriptorAndMove :: HSTMT a -> IO ColDescriptor
getCurrentColDescriptorAndMove hstmt = do
  let (ColPos wref) = colPos hstmt
  currColPos <- atomicModifyIORef' wref (\w -> (w +1, w))
  let actualColPos = fromIntegral currColPos
      expectedColSize = numResultCols hstmt
  when (expectedColSize < actualColPos) $
    throwIO (SQLNumResultColsException expectedColSize actualColPos)
  res <- sqlDescribeCol (getHSTMT hstmt) (fromIntegral currColPos)
  pure res

getCurrentColDescriptor :: HSTMT a -> IO ColDescriptor
getCurrentColDescriptor hstmt = do
  let (ColPos wref) = colPos hstmt
  currColPos <- readIORef wref
  sqlDescribeCol (getHSTMT hstmt) ( currColPos)

sqlMapping :: HM.HashMap TypeRep [SQLType]
sqlMapping =
  HM.fromList
  [ (typeOf (undefined :: CChar)     , [SQL_VARCHAR, SQL_LONGVARCHAR, SQL_CHAR, SQL_DECIMAL, SQL_LONGVARCHAR, SQL_WLONGVARCHAR, SQL_WVARCHAR])
  , (typeOf (undefined :: CUChar)    , [SQL_VARCHAR, SQL_LONGVARCHAR, SQL_CHAR])
  , (typeOf (undefined :: CWchar)    , [SQL_VARCHAR, SQL_LONGVARCHAR, SQL_CHAR, SQL_LONGVARCHAR, SQL_WLONGVARCHAR, SQL_WVARCHAR])
  , (typeOf (undefined :: CBinary)   , [SQL_LONGVARBINARY, SQL_VARBINARY])
  , (typeOf (undefined :: CText)     , [SQL_WLONGVARCHAR, SQL_VARCHAR, SQL_WVARCHAR, SQL_LONGVARCHAR])    
  , (typeOf (undefined :: CUTinyInt) , [SQL_TINYINT])
  , (typeOf (undefined :: CTinyInt)  , [SQL_TINYINT])
  , (typeOf (undefined :: CLong)     , [SQL_INTEGER])
  , (typeOf (undefined :: CULong)    , [SQL_INTEGER])
  , (typeOf (undefined :: CSmallInt) , [SQL_SMALLINT])
  , (typeOf (undefined :: CUSmallInt), [SQL_SMALLINT])
  , (typeOf (undefined :: CFloat)    , [SQL_REAL, SQL_DECIMAL])
  , (typeOf (undefined :: CDouble)   , [SQL_FLOAT, SQL_DECIMAL])
  , (typeOf (undefined :: CBool)     , [SQL_BIT])
  , (typeOf (undefined :: CDate)     , [SQL_DATE, SQL_TYPE_DATE])
  , (typeOf (undefined :: CBigInt)   , [SQL_BIGINT])
  , (typeOf (undefined :: CUBigInt)  , [SQL_BIGINT])
  , (typeOf (undefined :: CTimeOfDay), [SQL_TIME, SQL_SS_TIME2])
  , (typeOf (undefined :: CLocalTime), [SQL_TIMESTAMP, SQL_TYPE_TIMESTAMP])
  , (typeOf (undefined :: CZonedTime), [SQL_SS_TIMESTAMPOFFSET])
  , (typeOf (undefined :: UUID)      , [SQL_GUID])
  ]

withRetCode :: ResIndicator -> HandleRef -> a -> IO a
withRetCode ret ref a =
  case isSuccessful ret of
    True  -> pure a
    False -> getErrors ret ref >>= throwSQLException

bindColBuffer :: ColDescriptor -> ForeignPtr CLong -> ForeignPtr t -> ColBufferType 'BindCol t
bindColBuffer = BindColBuffer

getDataUnboundBuffer ::
  ColDescriptor ->
  CLong ->
  IO (IO ResIndicator, ForeignPtr t, ForeignPtr CLong) ->
  ColBufferType 'GetDataUnbound t
getDataUnboundBuffer = GetDataUnboundBuffer

getDataBoundBuffer ::
  ColDescriptor ->
  IO (ForeignPtr t, ForeignPtr CLong) ->
  ColBufferType 'GetDataBound t
getDataBoundBuffer = GetDataBoundBuffer

isSuccessful :: ResIndicator -> Bool
isSuccessful SQL_SUCCESS           = True
isSuccessful SQL_SUCCESS_WITH_INFO = True
isSuccessful _                     = False

peekFP :: Storable t => ForeignPtr t -> IO t
peekFP fp = withForeignPtr fp peek

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
               ret = SQLGetDiagRecW((SQLSMALLINT)$(int handleType), handle, ++i, eState, NULL, eMSG, SQL_MAX_MESSAGE_LENGTH, &eMSGLen);
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

-- NOTE: use SQLGetTypeInfo to get signed info
sqlDescribeCol :: Ptr SQLHSTMT -> CUShort -> IO ColDescriptor
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
               case isSuccessful ret  of
                 False -> getErrors ret (SQLSTMTRef hstmt) >>= throwSQLException
                 True -> do
                   nameLength <- peek nameLengthP
                   tableName <- fromPtr (castPtr tabNameP) (fromIntegral nameLength)
                   dataType <- peek dataTypeP
                   decimalDigits <- peek decimalDigitsP
                   cSize <- peek colSizeP
                   nullable <- peek nullableP
                   pure $ ColDescriptor
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


guardedMallocForeignPtrBytes :: Int -> IO (ForeignPtr a)
guardedMallocForeignPtrBytes = mallocForeignPtrBytes

