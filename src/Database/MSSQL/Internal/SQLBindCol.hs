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

module Database.MSSQL.Internal.SQLBindCol where

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
import Control.Monad.IO.Class
import qualified Data.Text as T
import Database.MSSQL.Internal.SQLError
import Data.Coerce
import Data.IORef
import Control.Monad
import Control.Exception (throwIO)
import Data.Word
import Data.Text.Foreign as T

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

class SQLBindCol t where
  sqlBindCol :: HSTMT t -> IO t

sqlBindColTpl :: forall t. (Typeable t) =>
                      HSTMT (ColBuffer (CColBind t)) ->
                      (Ptr SQLHSTMT -> ColDescriptor -> IO (ColBuffer (CColBind t))) ->
                      IO (ColBuffer (CColBind t))
sqlBindColTpl hstmt block = do
   let hstmtP = getHSTMT hstmt       
   cdesc <- getCurrentColDescriptorAndMove hstmt
   if match cdesc
     then block hstmtP cdesc
     else typeMismatch exps cdesc

     where match cdesc = colDataType cdesc `elem` exps
           exps        = maybe [] id (HM.lookup rep sqlMapping)
           rep         = typeOf (undefined :: t)

sqlBindColTplBound :: forall t. (Typeable t) =>
                      HSTMT (ColBuffer (CGetDataBound t)) ->
                      (Ptr SQLHSTMT -> ColDescriptor -> IO (ColBuffer (CGetDataBound t))) ->
                      IO (ColBuffer (CGetDataBound t))
sqlBindColTplBound hstmt block = do
   let hstmtP = getHSTMT hstmt       
   cdesc <- getCurrentColDescriptorAndMove hstmt
   if match cdesc
     then block hstmtP cdesc
     else typeMismatch exps cdesc

     where match cdesc = colDataType cdesc `elem` exps
           exps        = maybe [] id (HM.lookup rep sqlMapping)
           rep         = typeOf (undefined :: t)

sqlBindColTplUnbound :: forall t. (Typeable t) =>
                      HSTMT (ColBuffer (CGetDataUnbound t)) ->
                      (Ptr SQLHSTMT -> ColDescriptor -> IO (ColBuffer (CGetDataUnbound t))) ->
                      IO (ColBuffer (CGetDataUnbound t))
sqlBindColTplUnbound hstmt block = do
   let hstmtP = getHSTMT hstmt       
   cdesc <- getCurrentColDescriptorAndMove hstmt
   if match cdesc
     then block hstmtP cdesc
     else typeMismatch exps cdesc

     where match cdesc = colDataType cdesc `elem` exps
           exps        = maybe [] id (HM.lookup rep sqlMapping)
           rep         = typeOf (undefined :: t)

newtype CBinary = CBinary { getCBinary :: CUChar }
                deriving (Show, Eq, Ord, Enum, Bounded, Num, Integral, Real, Storable)

instance SQLBindCol (ColBuffer (CColBind CBinary)) where
  sqlBindCol hstmt =
    sqlBindColTpl hstmt $ \hstmtP cdesc -> do
      let
        cpos = colPosition cdesc
        bufSize = fromIntegral (colSize cdesc)
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

      returnWithRetCode ret (SQLSTMTRef hstmtP) $
        (ColBuffer (colBindBuffer lenOrIndFP (coerce binFP)))

instance SQLBindCol (ColBuffer (CGetDataUnbound CBinary)) where
  sqlBindCol hstmt = 
   sqlBindColTplUnbound hstmt block
   
     where block hstmtP cdesc = do
             let bufSize = fromIntegral (32 :: Int)
             binFP <- mallocForeignPtrBytes (fromIntegral bufSize)
             lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
             let cpos = colPosition cdesc
                 cbuf = getDataUnboundBuffer (\acc f ->
                                               fetchBytes hstmtP binFP lenOrIndFP bufSize cpos f acc)
                        
             pure $ (ColBuffer cbuf)

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
                           putStrLn $ "status: " ++ show (ret, lengthOrInd)
                           acc' <- withForeignPtr binFP $ \tptr -> f bufSize lengthOrInd (coerce tptr) acc
                           go acc'
                       False -> pure acc

instance SQLBindCol (ColBuffer (CGetDataUnbound CChar)) where
  sqlBindCol hstmt = 
   sqlBindColTplUnbound hstmt block
   
     where block hstmtP cdesc = do
             let bufSize = fromIntegral (20 :: Int)
             chrFP <- mallocForeignPtrBytes (fromIntegral bufSize)
             lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
             let cpos = colPosition cdesc
                 cbuf = getDataUnboundBuffer (\acc f ->
                                               fetchBytes hstmtP chrFP lenOrIndFP bufSize cpos f acc)
                        
             pure $ (ColBuffer cbuf)

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
                           {-
                           let actBufSize = case fromIntegral lengthOrInd of
                                              SQL_NO_TOTAL                 -> bufSize - 1
                                              i | i >= fromIntegral bufSize -> bufSize - 1
                                              _                            -> lengthOrInd
                           -}
                           acc' <- withForeignPtr chrFP $ \tptr -> f bufSize lengthOrInd (coerce tptr) acc
                           go acc'
                       False -> pure acc

instance SQLBindCol (ColBuffer (CColBind CChar)) where
  sqlBindCol hstmt =
    sqlBindColTpl hstmt $ \hstmtP cdesc -> do
      let
        cpos = colPosition cdesc
        bufSize = {-fromIntegral $-} (fromIntegral (colSize cdesc + 1)) -- colSizeAdjustment hstmt (fromIntegral (colSize cdesc + 1))
      -- print $ "colSize as said: " ++ show (colSize cdesc)
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

      returnWithRetCode ret (SQLSTMTRef hstmtP) $
        (ColBuffer (colBindBuffer lenOrIndFP (coerce chrFP)) )

instance SQLBindCol (ColBuffer (CColBind CWchar)) where
  sqlBindCol hstmt =
    sqlBindColTpl hstmt $ \hstmtP cdesc -> do
      let cpos = colPosition cdesc
          bufSize = fromIntegral (colSize cdesc * 4 + 1) -- colSizeAdjustment hstmt (fromIntegral (colSize cdesc + 1))
      print $ "colSize as said: " ++ show (colSize cdesc, bufSize)          
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
      peekFP lenOrIndFP >>= \a -> putStrLn $ "LenOrInd: " ++ show a
      returnWithRetCode ret (SQLSTMTRef hstmtP) $
        (ColBuffer (colBindBuffer lenOrIndFP $ coerce txtFP))

instance SQLBindCol (ColBuffer (CGetDataUnbound CWchar)) where
  sqlBindCol hstmt = 
   sqlBindColTplUnbound hstmt block
   
     where block hstmtP cdesc = do
             let bufSize = fromIntegral (36 :: Int)
             txtFP <- mallocForeignPtrBytes (fromIntegral bufSize)
             lenOrIndFP :: ForeignPtr CLong <- mallocForeignPtr
             let cpos = colPosition cdesc
                 cbuf = getDataUnboundBuffer (\acc f ->
                                               fetchText hstmtP txtFP lenOrIndFP bufSize cpos f acc)
                        
             pure $ (ColBuffer cbuf)

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
                           msgs <- getMessages (SQLSTMTRef hstmtP)
                           putStrLn $ "Message: " ++ show (msgs, ret, ret == SQL_SUCCESS)
                           lengthOrInd <- peekFP lenOrIndFP
                           {-
                           let actBufSize = case fromIntegral lengthOrInd of
                                              SQL_NO_TOTAL -> bufSize - 2
                                              _            -> lengthOrInd
                           -}
                           acc' <- withForeignPtr txtFP $ \tptr -> f bufSize lengthOrInd (coerce tptr) acc
                           go acc'
                       False -> pure acc

{-
newtype CDecimal a = CDecimal a
  deriving (Show, Eq, Ord, Enum, Bounded, Num, Integral, Real, Storable)

instance SQLBindCol (ColBuffer (CDecimal CDouble)) where
  sqlBindCol hstmt = do
    ebc <- sqlBindCol (coerce hstmt :: HSTMT (ColBuffer CDouble))
    pure (fmap (ColBuffer . castColBufferPtr . getColBuffer) ebc)

instance SQLBindCol (ColBuffer (CDecimal CFloat)) where
  sqlBindCol hstmt = do
    ebc <- sqlBindCol (coerce hstmt :: HSTMT (ColBuffer CFloat))
    pure (fmap (ColBuffer . castColBufferPtr . getColBuffer) ebc)
-}
{-
instance SQLBindCol (ColBuffer (CDecimal CChar)) where
  sqlBindCol hstmt = do
    ebc <- sqlBindCol (coerce (adjustColSize (+2) hstmt) :: HSTMT (ColBuffer CChar))
    pure (fmap (ColBuffer . castColBufferPtr . getColBuffer) ebc)
-}
{-
instance (KnownNat n) => SQLBindCol (ColBuffer (CSized n CChar)) where
  sqlBindCol hstmt = do
    ebc <- sqlBindCol (coerce hstmt :: HSTMT (ColBuffer CChar))
    pure (fmap (ColBuffer . castColBufferPtr . getColBuffer) ebc)

instance (KnownNat n) => SQLBindCol (ColBuffer (CSized n CWchar)) where
  sqlBindCol hstmt = do
    ebc <- sqlBindCol (coerce hstmt :: HSTMT (ColBuffer CWchar))
    pure (fmap (ColBuffer . castColBufferPtr . getColBuffer) ebc)
-}
{-
instance (KnownNat n) => SQLBindCol (ColBuffer (CSized n CBinary)) where
  sqlBindCol hstmt = do
    ebc <- sqlBindCol (coerce (adjustColSize (const bufSize) hstmt) :: HSTMT (ColBuffer CBinary))
    pure (fmap (ColBuffer . castColBufferPtr . getColBuffer) ebc)

    where bufSize = fromIntegral (natVal (Proxy :: Proxy n))

instance (KnownNat n) => SQLBindCol (ColBuffer (CSized n CWchar)) where
  sqlBindCol hstmt = do
    ebc <- sqlBindCol (coerce (adjustColSize (const bufSize) hstmt) :: HSTMT (ColBuffer CWchar))
    pure (fmap (ColBuffer . castColBufferPtr . getColBuffer) ebc)

    where bufSize = fromIntegral ((natVal (Proxy :: Proxy n)) * 4) + 2

instance (KnownNat n) => SQLBindCol (ColBuffer (CSized n (CDecimal CChar))) where
  sqlBindCol hstmt = do
    ebc <- sqlBindCol (coerce (adjustColSize (const bufSize) hstmt) :: HSTMT (ColBuffer CChar))
    pure (fmap (ColBuffer . castColBufferPtr . getColBuffer) ebc)

    where bufSize = fromIntegral (natVal (Proxy :: Proxy n)) + 3
-}

newtype CUTinyInt = CUTinyInt CUChar
                  deriving (Show, Eq, Ord, Enum, Bounded, Num, Integral, Real, Storable)

instance SQLBindCol (ColBuffer (CColBind CUTinyInt)) where
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
        returnWithRetCode ret (SQLSTMTRef hstmtP) $
          (ColBuffer (colBindBuffer lenOrIndFP $ castForeignPtr chrFP))

instance SQLBindCol (ColBuffer (CGetDataBound CUTinyInt)) where
  sqlBindCol hstmt =  
   sqlBindColTplBound hstmt $ \hstmtP cdesc -> pure $ ColBuffer $ getDataBoundBuffer $ do
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
        returnWithRetCode ret (SQLSTMTRef hstmtP) $
          (coerce chrFP, lenOrIndFP)


newtype CTinyInt = CTinyInt CChar
                  deriving (Show, Eq, Ord, Enum, Bounded, Num, Integral, Real, Storable)

instance SQLBindCol (ColBuffer (CColBind CTinyInt)) where
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
      returnWithRetCode ret (SQLSTMTRef hstmtP) $
        (ColBuffer (colBindBuffer lenOrIndFP $ castForeignPtr chrFP))

instance SQLBindCol (ColBuffer (CColBind CLong)) where
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

      returnWithRetCode ret (SQLSTMTRef hstmtP) $
        (ColBuffer (colBindBuffer lenOrIndFP (coerce intFP)))


instance SQLBindCol (ColBuffer (CGetDataBound CLong)) where
  sqlBindCol hstmt = 
   sqlBindColTplBound hstmt $ \hstmtP cdesc -> pure $ ColBuffer $ getDataBoundBuffer $ do
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

      returnWithRetCode ret (SQLSTMTRef hstmtP) $
        (coerce intFP, lenOrIndFP)

instance SQLBindCol (ColBuffer (CColBind CULong)) where
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

      returnWithRetCode ret (SQLSTMTRef hstmtP) $
        (ColBuffer (colBindBuffer lenOrIndFP (coerce intFP)))

newtype CSmallInt = CSmallInt CShort
                  deriving (Show, Eq, Ord, Enum, Bounded, Num, Integral, Real, Storable)

instance SQLBindCol (ColBuffer (CColBind CSmallInt)) where
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

       returnWithRetCode ret (SQLSTMTRef hstmtP) $
         (ColBuffer (colBindBuffer lenOrIndFP $ coerce shortFP))

instance SQLBindCol (ColBuffer (CGetDataBound CSmallInt)) where
  sqlBindCol hstmt = 
   sqlBindColTplBound hstmt $ \hstmtP cdesc -> pure $ ColBuffer $ getDataBoundBuffer $ do 
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

       returnWithRetCode ret (SQLSTMTRef hstmtP) $
         (coerce shortFP, lenOrIndFP)


newtype CUSmallInt = CUSmallInt CShort
                  deriving (Show, Eq, Ord, Enum, Bounded, Num, Integral, Real, Storable)

instance SQLBindCol (ColBuffer (CColBind CUSmallInt)) where
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

      returnWithRetCode ret (SQLSTMTRef hstmtP) $
        (ColBuffer (colBindBuffer lenOrIndFP $ coerce shortFP))

instance SQLBindCol (ColBuffer (CColBind CFloat)) where
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

      returnWithRetCode ret (SQLSTMTRef hstmtP) $
        (ColBuffer (colBindBuffer lenOrIndFP (coerce floatFP)))

instance SQLBindCol (ColBuffer (CGetDataBound CFloat)) where
  sqlBindCol hstmt =
   sqlBindColTplBound hstmt $ \hstmtP cdesc -> pure $ ColBuffer $ getDataBoundBuffer $ do
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

      returnWithRetCode ret (SQLSTMTRef hstmtP) $
        (coerce floatFP, lenOrIndFP)

instance SQLBindCol (ColBuffer (CColBind CDouble)) where
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
      returnWithRetCode ret (SQLSTMTRef hstmtP) $
        (ColBuffer (colBindBuffer lenOrIndFP (coerce floatFP)))

instance SQLBindCol (ColBuffer (CGetDataBound CDouble)) where
  sqlBindCol hstmt =
   sqlBindColTplBound hstmt $ \hstmtP cdesc -> pure $ ColBuffer $ getDataBoundBuffer $ do
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
      returnWithRetCode ret (SQLSTMTRef hstmtP) $
        (coerce floatFP, lenOrIndFP)

instance SQLBindCol (ColBuffer (CColBind CBool)) where
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
      returnWithRetCode ret (SQLSTMTRef hstmtP) $
        (ColBuffer (colBindBuffer lenOrIndFP $ castForeignPtr chrFP))

instance SQLBindCol (ColBuffer (CGetDataBound CBool)) where
  sqlBindCol hstmt =
   sqlBindColTplBound hstmt $ \hstmtP cdesc -> pure $ ColBuffer $ getDataBoundBuffer $ do    
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
      returnWithRetCode ret (SQLSTMTRef hstmtP) $
        (coerce chrFP, lenOrIndFP)

instance SQLBindCol (ColBuffer (CColBind CDate)) where
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

      returnWithRetCode ret (SQLSTMTRef hstmtP) $
        (ColBuffer (colBindBuffer lenOrIndFP (coerce dateFP)))

instance SQLBindCol (ColBuffer (CGetDataBound CDate)) where
  sqlBindCol hstmt = do
   sqlBindColTplBound hstmt $ \hstmtP cdesc -> pure $ ColBuffer $ getDataBoundBuffer $ do    
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

      returnWithRetCode ret (SQLSTMTRef hstmtP) $
         (coerce dateFP, lenOrIndFP)

newtype CBigInt = CBigInt CLLong
                  deriving (Show, Eq, Ord, Enum, Bounded, Num, Integral, Real, Storable)

instance SQLBindCol (ColBuffer (CColBind CBigInt)) where
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

      returnWithRetCode ret (SQLSTMTRef hstmtP) $
        (ColBuffer (colBindBuffer lenOrIndFP $ coerce llongFP))

instance SQLBindCol (ColBuffer (CGetDataBound CBigInt)) where
  sqlBindCol hstmt =
      sqlBindColTplBound hstmt $ \hstmtP cdesc -> pure $ ColBuffer $ getDataBoundBuffer $ do
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

       returnWithRetCode ret (SQLSTMTRef hstmtP) $
         (coerce llongFP, lenOrIndFP)

newtype CUBigInt = CUBigInt CULLong
                  deriving (Show, Eq, Ord, Enum, Bounded, Num, Integral, Real, Storable)

instance SQLBindCol (ColBuffer (CColBind CUBigInt)) where
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

     returnWithRetCode ret (SQLSTMTRef hstmtP) $
       (ColBuffer (colBindBuffer lenOrIndFP $ coerce llongFP))

instance SQLBindCol (ColBuffer (CColBind CTimeOfDay)) where
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

     returnWithRetCode ret (SQLSTMTRef hstmtP) $
       (ColBuffer (colBindBuffer lenOrIndFP (coerce todFP)))

instance SQLBindCol (ColBuffer (CGetDataBound CTimeOfDay)) where
  sqlBindCol hstmt = do
   sqlBindColTplBound hstmt $ \hstmtP cdesc -> pure $ ColBuffer $ getDataBoundBuffer $ do
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

     returnWithRetCode ret (SQLSTMTRef hstmtP) $
       (coerce todFP, lenOrIndFP)

instance SQLBindCol (ColBuffer (CColBind CLocalTime)) where
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
       returnWithRetCode ret (SQLSTMTRef hstmtP) $
         (ColBuffer (colBindBuffer lenOrIndFP (coerce ltimeFP)))

instance SQLBindCol (ColBuffer (CGetDataBound CLocalTime)) where
  sqlBindCol hstmt = do
   sqlBindColTplBound hstmt $ \hstmtP cdesc -> pure $ ColBuffer $ getDataBoundBuffer $ do
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
       returnWithRetCode ret (SQLSTMTRef hstmtP) $
         (coerce ltimeFP, lenOrIndFP)

instance SQLBindCol (ColBuffer (CColBind CZonedTime)) where
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

     returnWithRetCode ret (SQLSTMTRef hstmtP) $
       (ColBuffer (colBindBuffer lenOrIndFP (coerce ltimeFP)))

instance SQLBindCol (ColBuffer (CGetDataBound CZonedTime)) where
  sqlBindCol hstmt =
   sqlBindColTplBound hstmt $ \hstmtP cdesc -> pure $ ColBuffer $ getDataBoundBuffer $ do    
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

     returnWithRetCode ret (SQLSTMTRef hstmtP) $
       (coerce ltimeFP, lenOrIndFP)

instance SQLBindCol (ColBuffer (CColBind UUID)) where
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

     returnWithRetCode ret (SQLSTMTRef hstmtP) $
       (ColBuffer (colBindBuffer lenOrIndFP (coerce uuidFP)))

typeMismatch :: (MonadIO m) => [SQLType] -> ColDescriptor -> m a
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
  in  throwSQLException (SQLErrors [ se ])

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

sqlMapping :: HM.HashMap TypeRep [SQLType]
sqlMapping =
  HM.fromList
  [ (typeOf (undefined :: CChar)     , [SQL_VARCHAR, SQL_LONGVARCHAR, SQL_CHAR, SQL_DECIMAL, SQL_LONGVARCHAR, SQL_WLONGVARCHAR, SQL_WVARCHAR])
  , (typeOf (undefined :: CUChar)    , [SQL_VARCHAR, SQL_LONGVARCHAR, SQL_CHAR])
  , (typeOf (undefined :: CWchar)    , [SQL_VARCHAR, SQL_LONGVARCHAR, SQL_CHAR, SQL_LONGVARCHAR, SQL_WLONGVARCHAR, SQL_WVARCHAR])
  , (typeOf (undefined :: CBinary)   , [SQL_LONGVARBINARY, SQL_VARBINARY, SQL_WLONGVARCHAR, SQL_VARCHAR, SQL_WVARCHAR, SQL_LONGVARCHAR])  
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

returnWithRetCode :: ResIndicator -> HandleRef -> a -> IO a
returnWithRetCode ret ref a =
  case isSuccessful ret of
    True  -> pure a
    False -> getErrors ret ref >>= throwSQLException

unboundWith :: 
  Storable t =>
  ColBufferType 'GetDataUnbound t ->
  a ->
  (CLong -> CLong -> Ptr t -> a -> IO a) ->
  IO a
unboundWith cbuff a f =
  case cbuff of
    GetDataUnboundBuffer k -> k a f

colBindBuffer :: ForeignPtr CLong -> ForeignPtr t -> ColBufferType 'ColBind t
colBindBuffer = ColBindBuffer

getDataUnboundBuffer ::
  (forall a. a -> (CLong -> CLong -> Ptr t -> a -> IO a) -> IO a) ->
  ColBufferType 'GetDataUnbound t
getDataUnboundBuffer = GetDataUnboundBuffer

getDataBoundBuffer ::
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
             printf("%d", SQL_MAX_MESSAGE_LENGTH);
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
  putStrLn $  "in get errors: " ++ show res
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
