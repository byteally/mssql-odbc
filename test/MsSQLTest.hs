{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeFamilies        #-}

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
import qualified Data.HashMap.Strict as HM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Functor.Compose

import qualified Data.String as S

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


data Album = Album Int32 Text Int32
           deriving (Generic, Show)

instance FromRow Album

-- connectInfo (ConnectionString {})
-- connectInfo ""

-- testConnectInfo :: ConnectInfo  
-- testConnectInfo = connectInfo "Driver={ODBC Driver 17 for SQL Server};Server=localhost;Database=Chinook;UID=sa;PWD=p@ssw0rd;ApplicationIntent=ReadOnly"

localConnectionStr :: ConnectionString 
localConnectionStr =
  ConnectionString { database          = "Chinook"
                   , server            = "localhost"
                   , port              = 1433
                   , user              = "sa"
                   , password          = "p@ssw0rd"
                   , odbcDriver        = odbcSQLServer17
                   , connectProperties = HM.singleton "ApplicationIntent" "ReadOnly"
                   }

testConnectInfo :: ConnectInfo
testConnectInfo = connectInfo localConnectionStr

unit_connect :: IO ()
unit_connect = do
  let conInfo = testConnectInfo {attrBefore = SV.fromList [SQL_ATTR_ACCESS_MODE, SQL_ATTR_AUTOCOMMIT]}
  Right con <- connect conInfo
  res <- query con "select * from Album" :: IO (Either SQLErrors (Vector Album))
  res1 <- runSession testConnectInfo $ query_ "select img from test"

  print res
  case res1 :: Either SQLErrors (Vector (Identity Image)) of
    Right vs        ->
      mapM (BS.writeFile "/tmp/output.png" . getImage . runIdentity) vs >> pure ()
    Left es         -> print es
  disconnect con
  pure ()

_unit_sqlinsert :: IO ()
_unit_sqlinsert = do
  res <- runSession testConnectInfo $ do
    execute_ "insert test1 (test_id, col1, col2, col3, col4, smallint, bit, tinyint, bigint, dbl, flt, datec, tod, dt, dt2, sdt, dtz, utc1, uuid, ntxt, char10, nchar10) VALUES (5, '12/13/2012', 3, 'fdfd', 'fdffddf', 32, 1, 23,99999999,4, 6.5,'1/13/2013', '00:00:00', '2015-03-19 05:15:18.123', '2015-03-19 05:15:18.123', '2015-03-19 05:15:18.123', '2015-03-19 05:15:18.123+05:30', '2015-03-19 05:15:18.123+05:30', '0E984725-C51C-4BF4-9960-E1C80E27ABA0', N'🌀', 'dfd', N'🌀');"
  print res

test_roundTrip :: TestTree
test_roundTrip =
  withResource (connect testConnectInfo {attrBefore = SV.fromList [SQL_ATTR_ACCESS_MODE, SQL_ATTR_AUTOCOMMIT]})
               (\h -> case h of
                   Right h' -> do
                     disconnect h'
                     pure ()
                   Left _ -> pure ()
               ) $ \r -> 
  testGroup "round trip tests"
  [ testProperty "maxBound @Int" $ withTests 100 $ roundTrip r (Gen.int $ Range.singleton $ maxBound @Int)
  , testProperty "minBound @Int" $ withTests 100 $ roundTrip r (Gen.int $ Range.singleton $ minBound @Int)
  
  -- -- , testProperty "maxBound @Int8" $ withTests 1 $ roundTrip r (Gen.int8 $ Range.singleton $ maxBound @Int8)
  -- -- , testProperty "minBound @Int8" $ withTests 1 $ roundTrip r (Gen.int8 $ Range.singleton $ minBound @Int8)

  , testProperty "maxBound @Int16" $ withTests 100 $ roundTrip r (Gen.int16 $ Range.singleton $ maxBound @Int16)
  , testProperty "minBound @Int16" $ withTests 100 $ roundTrip r (Gen.int16 $ Range.singleton $ minBound @Int16)

  , testProperty "maxBound @Int32" $ withTests 100 $ roundTrip r (Gen.int32 $ Range.singleton $ maxBound @Int32)
  , testProperty "minBound @Int32" $ withTests 100 $ roundTrip r (Gen.int32 $ Range.singleton $ minBound @Int32)

  , testProperty "maxBound @Int64" $ withTests 100 $ roundTrip r (Gen.int64 $ Range.singleton $ maxBound @Int64)
  , testProperty "minBound @Int64" $ withTests 100 $ roundTrip r (Gen.int64 $ Range.singleton $ minBound @Int64)

  , testProperty "minBound @Money" $ withTests 100 $ roundTrip r (toMoney <$> (Gen.int64 $ Range.singleton $ minBound @Int64))  
  , testProperty "maxBound @Money" $ withTests 100 $ roundTrip r (toMoney <$> (Gen.int64 $ Range.singleton $ maxBound @Int64))  

  , testProperty "minBound @SmallMoney" $ withTests 100 $ roundTrip r (toSmallMoney <$> (Gen.int32 $ Range.singleton $ minBound @Int32))  -- OK
  , testProperty "maxBound @SmallMoney" $ withTests 100 $ roundTrip r (toSmallMoney <$> (Gen.int32 $ Range.singleton $ maxBound @Int32))  -- OK
  
  -- , testProperty "maxBound @Word" $ withTests 100 $ roundTrip r (Gen.word $ Range.singleton $ maxBound @Word)
  -- , testProperty "minBound @Word" $ withTests 100 $ roundTrip r (Gen.word $ Range.singleton $ minBound @Word)

  -- , testProperty "maxBound @Word8" $ withTests 100 $ roundTrip r (Gen.word8 $ Range.singleton $ maxBound @Word8)
  -- , testProperty "minBound @Word8" $ withTests 100 $ roundTrip r (Gen.word8 $ Range.singleton $ minBound @Word8)

  -- , testProperty "maxBound @Word16" $ withTests 100 $ roundTrip r (Gen.word16 $ Range.singleton $ maxBound @Word16)
  -- , testProperty "minBound @Word16" $ withTests 100 $ roundTrip r (Gen.word16 $ Range.singleton $ minBound @Word16)

  -- , testProperty "maxBound @Word32" $ withTests 100 $ roundTrip r (Gen.word32 $ Range.singleton $ maxBound @Word32)
  -- , testProperty "minBound @Word32" $ withTests 100 $ roundTrip r (Gen.word32 $ Range.singleton $ minBound @Word32)

  -- , testProperty "maxBound @Word64" $ withTests 100 $ roundTrip r (Gen.word64 $ Range.singleton $ maxBound @Word64)
  -- , testProperty "minBound @Word64" $ withTests 100 $ roundTrip r (Gen.word64 $ Range.singleton $ minBound @Word64)

  , testProperty "@Bool" $ roundTrip r ((\b -> if b then 1 else 0 :: Word8) <$> Gen.bool) -- OK
  , testProperty "@Day" $ withTests 100 $ roundTrip r (day) -- OK
  , testProperty "@TimeOfDay" $ withTests 100 $ roundTrip r (timeOfDay)
  , testProperty "@LocalTime" $ withTests 100 $ roundTrip r (localTime) -- OK
  , testProperty "@Float" $ withTests 100 $ roundTrip r (Gen.float $ Range.exponentialFloat (-100) 100) -- OK
  , testProperty "@Double" $ withTests 100 $ roundTrip r (Gen.double $ Range.exponentialFloat (-100) 100) -- OK
  , testProperty "@ASCIIText" $ withTests 100 $ roundTripWith r asciiText getAsciiText
    -- --hedgehog-replay "Size 0 Seed 16971920948893089903 7310332839496341167"
  , testProperty "double reg" $ property $ do
      v <- evalIO $ runSession testConnectInfo $
        query_ "select CAST ( -100.0 AS FLOAT(53))"
      v === Right (V.fromList [Identity (-100.0 :: Double)])
  ]

  where toMoney i64 = Money (read (show i64) / 10000)
        toSmallMoney i32 = SmallMoney (read (show i32) / 10000)  

asciiText :: MonadGen m => m ASCIIText
asciiText =
  (ASCIIText . T.pack . escapeQuotes) <$> Gen.list (Range.constant 0 1000) Gen.ascii

  where escapeQuotes =
          foldr (\a acc -> case a of
                    '\'' -> "''" <> acc
                    _    -> a : acc
                ) []
  
  
day :: ( MonadGen m
      , GenBase m ~ Identity
      ) => m Day
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
timeOfDay :: ( MonadGen m
             , GenBase m ~ Identity
             ) => m TimeOfDay
timeOfDay = Gen.just $ do
  h <- Gen.enum 0 24
  m <- Gen.enum 0 59
  (s :: Int) <- Gen.enum 0 59 -- TODO: Handle pico
  pure $ makeTimeOfDayValid h m (fromRational $ toRational s)

localTime :: ( MonadGen m
             , GenBase m ~ Identity
             ) => m LocalTime
localTime = do
  dy <- day
  tod <- timeOfDay
  pure $ LocalTime dy tod

roundTrip :: forall a.
  ( Show a
  , Eq a
  , FromField a
  , Typeable a
  ) => IO (Either SQLErrors Connection) -> Gen a -> Property
roundTrip con gen =
  roundTripWith con gen (T.pack . show)

roundTripWith :: forall a.
  ( Eq a
  , FromField a
  , Typeable a
  , Show a
  ) => IO (Either SQLErrors Connection) -> Gen a -> (a -> Text) -> Property
roundTripWith con gen f =
  property $ do
    val <- forAll gen
    trippingM val mkQuery evalQuery
    pure ()

    where mkQuery a =
            let fmtedVal =
                  if isQuoted
                     then "'" <> f a <> "'"
                     else f a 
                (sqlType, isQuoted) = getSQLType a
            in eval $ S.fromString (T.unpack ("select CAST (" <> fmtedVal <> " AS " <> sqlType <> ")"))

          evalQuery q =
            evalIO . fmap wrapCompose $ do
                Right con' <- con
                query con' q

          wrapCompose :: forall a b.
                        Either a (Vector (Identity b)) ->
                        Compose (Either a) (Compose Vector Identity) b
          wrapCompose = Compose . fmap Compose

{-
    let valTxt = f val
        sqlType = fst $ getSQLType gen
        isQuoted = snd $ getSQLType gen
        fmtedVal = if isQuoted then "'" <> valTxt <> "'" else valTxt
--    liftIO $ print $ "select CAST (" <> fmtedVal <> " AS " <> (sqlType) <> ")"
    r <- evalEither =<< (
      liftIO $ runSession testConnectInfo $ do
          -- liftIO $ print ("select CAST (" <> fmtedVal <> " AS " <> (sqlType) <> ")")
          r :: Vector (Identity a) <- query_ $ ("select CAST (" <> fmtedVal <> " AS " <> (sqlType) <> ")")
          pure r
      )

-}
          
getSQLType :: Typeable a => a -> (Text, Bool)
getSQLType a = case show $ typeOf a of
  "Int"    -> ("BIGINT", False)
  "Int16"  -> ("SMALLINT", False)
  "Int32"  -> ("INTEGER", False)
  "Int64"  -> ("BIGINT", False)
  "Word8"  -> ("TINYINT", False)
  "Bool"   -> ("BIT", False)
  "Day"    -> ("DATE", True)
  "TimeOfDay" -> ("TIME", True)
  "LocalTime" -> ("DATETIME2", True)
  "Float"     -> ("REAL", False)
  "Double"    -> ("FLOAT(53)", False)
  "Money"    -> ("MONEY", True)
  "SmallMoney" -> ("SMALLMONEY", True)  
  "ASCIIText" -> ("VARCHAR", True)  
  
{-
  "Int8"   -> "TINYINT"
  "Word"   -> "NUMERIC(20,0)"
  "Word16" -> "NUMERIC(5,0)"
  "Word32" -> "NUMERIC(10,0)"
  "Word64" -> "NUMERIC(20,0)"
-}
