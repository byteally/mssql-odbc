{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module MsSQLTest where

import Database.MsSQL
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Vector.Storable as SV
import GHC.Generics
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Int
import Data.Word
import Data.Time
import Data.UUID.Types
import Control.Monad.IO.Class
import Data.Semigroup

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty.Hedgehog
import Data.Functor.Identity
import Data.Typeable
import Data.Fixed


data TestT1 = TestT1
  { test_id :: Int32
  , col1 :: Text
  , col2 :: Maybe Int32
  , col3 :: Maybe ByteString
  , col4 :: Maybe Text
  , smallint :: Maybe Int16
  , bit :: Maybe Bool
  , tinyint :: Maybe Word8
  , bigint :: Maybe Int64
  , dbl :: Maybe Double
  , flt :: Maybe Float
  , datec :: Maybe Day
  , tod :: Maybe TimeOfDay
  , dt :: Maybe LocalTime
  , dt2 :: Maybe LocalTime
  , sdt :: Maybe LocalTime
  , dtz :: Maybe ZonedTime
  , utc' :: Maybe UTCTime
  , uuid :: Maybe UUID
  , ntxt :: Maybe Text
  , char10 :: Maybe ByteString
  , nchar10 :: Maybe Text
  } deriving (Generic, Show)


data Foo = Foo
  { f1 :: Int
  , f2 :: Double
  , f6 :: Float
  , f3 :: Char
  , f4 :: Bool
  } deriving (Generic)

instance FromRow TestT1

uc ::String
uc = "🌀"


testConnectInfo :: ConnectInfo  
testConnectInfo = connectInfo "Driver={ODBC Driver 17 for SQL Server};Server=localhost;Database=hask;UID=sa;PWD=P@ssw0rd;ApplicationIntent=ReadOnly"

unit_connect :: IO ()
unit_connect = do
  let conInfo = (testConnectInfo {attrBefore = SV.fromList [SQL_ATTR_ACCESS_MODE, SQL_ATTR_AUTOCOMMIT]})
  Right con <- connect conInfo
  res <- query con "select * from test1" :: IO (Either SQLErrors (Vector TestT1))
  print res
  disconnect con
  pure ()

unit_sqlinsert :: IO ()
unit_sqlinsert = do
  res <- runSession testConnectInfo $ do
    execute_ "insert test1 (test_id, col1, col2, col3, col4, smallint, bit, tinyint, bigint, dbl, flt, datec, tod, dt, dt2, sdt, dtz, utc1, uuid, ntxt, char10, nchar10) VALUES (5, '12/13/2012', 3, 'fdfd', 'fdffddf', 32, 1, 23,99999999,4, 6.5,'1/13/2013', '00:00:00', '2015-03-19 05:15:18.123', '2015-03-19 05:15:18.123', '2015-03-19 05:15:18.123', '2015-03-19 05:15:18.123+05:30', '2015-03-19 05:15:18.123+05:30', '0E984725-C51C-4BF4-9960-E1C80E27ABA0', N'🌀', 'dfd', N'🌀');"
  print res


test_roundTrip :: TestTree
test_roundTrip =
  testGroup "round trip tests"
  [ testProperty "maxBound @Int" $ withTests 1 $ roundTrip (Gen.int $ Range.singleton $ maxBound @Int)
  , testProperty "minBound @Int" $ withTests 1 $ roundTrip (Gen.int $ Range.singleton $ minBound @Int)
  
  -- , testProperty "maxBound @Int8" $ withTests 1 $ roundTrip (Gen.int8 $ Range.singleton $ maxBound @Int8)
  -- , testProperty "minBound @Int8" $ withTests 1 $ roundTrip (Gen.int8 $ Range.singleton $ minBound @Int8)

  , testProperty "maxBound @Int16" $ withTests 1 $ roundTrip (Gen.int16 $ Range.singleton $ maxBound @Int16)
  , testProperty "minBound @Int16" $ withTests 1 $ roundTrip (Gen.int16 $ Range.singleton $ minBound @Int16)

  , testProperty "maxBound @Int32" $ withTests 1 $ roundTrip (Gen.int32 $ Range.singleton $ maxBound @Int32)
  , testProperty "minBound @Int32" $ withTests 1 $ roundTrip (Gen.int32 $ Range.singleton $ minBound @Int32)

  , testProperty "maxBound @Int64" $ withTests 1 $ roundTrip (Gen.int64 $ Range.singleton $ maxBound @Int64)
  , testProperty "minBound @Int64" $ withTests 1 $ roundTrip (Gen.int64 $ Range.singleton $ minBound @Int64)
  
  -- , testProperty "maxBound @Word" $ withTests 1 $ roundTrip (Gen.word $ Range.singleton $ maxBound @Word)
  -- , testProperty "minBound @Word" $ withTests 1 $ roundTrip (Gen.word $ Range.singleton $ minBound @Word)

  -- , testProperty "maxBound @Word8" $ withTests 1 $ roundTrip (Gen.word8 $ Range.singleton $ maxBound @Word8)
  -- , testProperty "minBound @Word8" $ withTests 1 $ roundTrip (Gen.word8 $ Range.singleton $ minBound @Word8)

  -- , testProperty "maxBound @Word16" $ withTests 1 $ roundTrip (Gen.word16 $ Range.singleton $ maxBound @Word16)
  -- , testProperty "minBound @Word16" $ withTests 1 $ roundTrip (Gen.word16 $ Range.singleton $ minBound @Word16)

  -- , testProperty "maxBound @Word32" $ withTests 1 $ roundTrip (Gen.word32 $ Range.singleton $ maxBound @Word32)
  -- , testProperty "minBound @Word32" $ withTests 1 $ roundTrip (Gen.word32 $ Range.singleton $ minBound @Word32)

  -- , testProperty "maxBound @Word64" $ withTests 1 $ roundTrip (Gen.word64 $ Range.singleton $ maxBound @Word64)
  -- , testProperty "minBound @Word64" $ withTests 1 $ roundTrip (Gen.word64 $ Range.singleton $ minBound @Word64)

  , testProperty "@Bool" $ roundTrip ((\b -> if b then 1 else 0 :: Word8) <$> Gen.bool)
  , testProperty "@Day" $ withTests 100 $ roundTrip (day)
  , testProperty "@TimeOfDay" $ withTests 100 $ roundTrip (timeOfDay)
  , testProperty "@LocalTime" $ withTests 100 $ roundTrip (localTime)

  , testProperty "@Float" $ withTests 100 $ roundTrip (Gen.float $ Range.exponentialFloat (-100) 100)

  , testProperty "@Double" $ withTests 100 $ roundTrip (Gen.double $ Range.exponentialFloat (-100) 100)

  ]

day :: MonadGen m => m Day
day = Gen.just $ do
  dy <- Gen.enum 1 31
  m <- Gen.enum 1 12
  yr <- Gen.enum 2012 2016
  pure $ fromGregorianValid yr m dy

{- TODO:
* Valid range of second including leap seconds is 0 to 61
  - TIME has problem parsing 60 and 61 as sec
* TIME has problem with parsing fractional sec
-}
timeOfDay :: MonadGen m => m TimeOfDay
timeOfDay = Gen.just $ do
  h <- Gen.enum 0 24
  m <- Gen.enum 0 59
  (s :: Int) <- Gen.enum 0 59 -- TODO: Handle pico
  pure $ makeTimeOfDayValid h m (fromRational $ toRational s)

localTime :: MonadGen m => m LocalTime
localTime = do
  dy <- day
  tod <- timeOfDay
  pure $ LocalTime dy tod


roundTrip :: forall a.
  ( Show a
  , Eq a
  , FromField a
  , Typeable a
  , SQLBindCol (ColBuffer (FieldBufferType a))
  ) => Gen a -> Property
roundTrip gen =
  property $ do
    val <- forAll gen
    let valTxt = (T.pack $ show val)
        sqlType = fst $ getSQLType gen
        isQuoted = snd $ getSQLType gen
        fmtedVal = if isQuoted then "'" <> valTxt <> "'" else valTxt
--    liftIO $ print $ "select CAST (" <> fmtedVal <> " AS " <> (sqlType) <> ")"
    r <- evalEither =<< (
      liftIO $ runSession testConnectInfo $ do
          r :: Vector (Identity a) <- query_ $ ("select CAST (" <> fmtedVal <> " AS " <> (sqlType) <> ")")
          pure r
      )
    
    V.fromList [(Identity val)] === r

getSQLType :: Typeable a => gen a -> (Text, Bool)
getSQLType a = case show $ typeRep a of
  "Int"    -> ("BIGINT", False)
  "Int16"  -> ("SMALLINT", False)
  "Int32"  -> ("INTEGER", False)
  "Int64"  -> ("BIGINT", False)
  "Word8"  -> ("TINYINT", False)
  "Bool"   -> ("BIT(1)", False)
  "Day"    -> ("DATE", True)
  "TimeOfDay" -> ("TIME", True)
  "LocalTime" -> ("DATETIME2", True)
  "Float"     -> ("REAL", False)
  "Double"    -> ("FLOAT(53)", False)
{-
  "Int8"   -> "TINYINT"
  "Word"   -> "NUMERIC(20,0)"
  "Word16" -> "NUMERIC(5,0)"
  "Word32" -> "NUMERIC(10,0)"
  "Word64" -> "NUMERIC(20,0)"
-}
