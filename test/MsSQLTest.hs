{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE DataKinds           #-}

module MsSQLTest where

import Database.MSSQL
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
import qualified Data.ByteString.Lazy as BSL
import Data.Functor.Compose
import qualified Data.ByteString as B

import qualified Data.String as S
import Numeric
import Control.Exception
import Utils
import Data.Char

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

data DT_Overflow = DT_Overflow { dto1 :: Int, dto2 :: Double }
        deriving (Show, Eq, Generic)

instance FromRow DT_Overflow where
  -- fromRow = DT_Overflow <*> field <*> field

unit_transaction_test :: IO ()
unit_transaction_test = do
  let conInfo = testConnectInfo
  con <- connect conInfo
  execute con "delete from test"  
  withTransaction con $ do
    execute con "insert into test (value) values (1)"
  (vs :: Vector (Identity Int32)) <- query con "select value from test"  
  disconnect con
  pure (pure 1) @=? vs  
  pure ()

unit_transaction_prim_test :: IO ()
unit_transaction_prim_test = do
  let conInfo = testConnectInfo
  con <- connect conInfo
  execute con "delete from test"
  setAutoCommitOff (_hdbc con)
  execute con "insert into test (value) values (1)"
  rollback (_hdbc con)
  (vs :: Vector (Identity Int32)) <- query con "select value from test"
  mempty @=? vs
  execute con "insert into test (value) values (1)"
  commit (_hdbc con)
  (vs :: Vector (Identity Int32)) <- query con "select value from test"
  setAutoCommitOn (_hdbc con)  
  disconnect con
  pure (pure 1) @=? vs
  
_unit_instance_overflow :: IO ()
_unit_instance_overflow = do
  let conInfo = testConnectInfo
  con <- connect conInfo
  print "instance overflow test"
  res <- query con "select CAST(54 AS BIGINT), CAST(96.2142 AS FLOAT(53)), 'takashis castle')" :: IO (Vector DT_Overflow)
  print res
  disconnect con
  pure ()

data DT_Underflow = DT_Underflow { dtu1 :: Int, dtu2 :: Double }
        deriving (Show, Eq, Generic)

instance FromRow DT_Underflow where
  -- fromRow = DT_Underflow <*> field <*> field

_unit_instance_underflow :: IO ()
_unit_instance_underflow = do
  let conInfo = testConnectInfo
  con <- connect conInfo
  print "instance overflow test"
  res <- query con "select CAST (54 AS BIGINT)" :: IO (Vector DT_Underflow)
  print res
  disconnect con
  pure ()


-- NOTE: This fails on sqlserver 2012
_unit_double :: IO ()
_unit_double = do
  let conInfo = testConnectInfo
  con <- connect conInfo
  print "float test"
  let v = -99.99999999999979
  res <- query con "select CAST (-99.99999999999979 AS FLOAT(53))" :: IO (Vector (Identity Double))
  print res
  pure (pure v) @=? res
  disconnect con
  pure ()

unit_maybe :: IO ()
unit_maybe = do
  let conInfo = testConnectInfo
  con <- connect conInfo
  print "maybe test"
  res <- query con "select CAST (null AS BIGINT) as foo" :: IO (Vector (Identity (Maybe Int)))
  print res
  pure (pure Nothing) @=? res
  disconnect con
  pure ()

unit_bytestring :: IO ()
unit_bytestring = do
  let conInfo = testConnectInfo
  con <- connect conInfo
  print "bytestring test"
  let t = "0x7f5145d98ffffffffffff"
  res <- query con ("select CAST(" <> t <> " AS VARBINARY)") :: IO (Vector (Identity ByteString))
  pure (pure t) @=? ((fmap . fmap) (\v -> "0x" <> T.pack (B.foldr showHex "" v)) res)
  disconnect con
  pure ()


unit_ascii :: IO ()
unit_ascii = do
  let conInfo = testConnectInfo
  con <- connect conInfo
  print "ascii test"
  res <- query con "select CAST ('((((((((((((((((((((' AS VARCHAR(5000))" :: IO (Vector (Identity ASCIIText))
  pure (pure "((((((((((((((((((((") @=? res
  disconnect con
  pure ()

unit_text :: IO ()
unit_text = do
  let conInfo = testConnectInfo
  con <- connect conInfo
  let t1 = ""
  print "bbbbb test"
  res <- query con "select CAST (N'' AS NTEXT)" :: IO (Vector (Identity Text))
  res @=? pure (pure "")
  res <- query con "select CAST (N'ASDFDASGASD54234asdas;;@' AS NTEXT)" :: IO (Vector (Identity Text))
  res @=? pure (pure "ASDFDASGASD54234asdas;;@")  
  res1 <- query con "select CAST (N'岧㊪藺绩鉒쀷꯶隇㏛骝⠃騍' AS NTEXT)" :: IO (Vector (Identity Text))
  res1 @=? pure (pure "岧㊪藺绩鉒쀷꯶隇㏛骝⠃騍")
  -- res1 <- query con "select CAST (N'򑏕󏔞󿅿󯗛􋭠𾩆񿁽􁾄񧨡򐗃򼂈򁤭񈐬𾦌󨀁򊘣򯼍񔆶傤򀪑򵟩􌰬󍗟𝓢󍍍𪩮󲥈󾩓󦼢񝃉򍗔𤩴򓮵򻗞󔳛󛷁򦇑񋤧򣼘񎾩񀂦򠼸򸈂󚩺򲕝񕻝񞝂򡐿󁱛󷊜' AS NVARCHAR(50))" :: IO (Vector (Identity (Sized 50 Text)))
  -- res1 @=? pure (pure "򑏕󏔞󿅿󯗛􋭠𾩆񿁽􁾄񧨡򐗃򼂈򁤭񈐬𾦌󨀁򊘣򯼍񔆶傤򀪑򵟩􌰬󍗟𝓢󍍍𪩮󲥈󾩓󦼢񝃉򍗔𤩴򓮵򻗞󔳛󛷁򦇑񋤧򣼘񎾩񀂦򠼸򸈂󚩺򲕝񕻝񞝂򡐿󁱛󷊜")
  res1 <- query con "select CAST (N'ABCDEFGHIJKLMNOP' AS NVARCHAR(16))" :: IO (Vector (Identity (Sized 16 Text)))
  res1 @=? pure (pure "ABCDEFGHIJKLMNOP")
  res1 <- query con "select CAST (N'large world of text which is like really huge' AS NVARCHAR(50))" :: IO (Vector (Identity Text))
  res1 @=? pure (pure "large world of text which is like really huge")

  res1 <- queryMany con "EXEC ChinookSP1"
  print (res1 :: (Identity (Vector Triple)))

  res1 <- queryMany con "EXEC ChinookSP2"
  print (res1 :: (Vector Triple, (Vector (Identity Text), ())))
  
  disconnect con
  pure ()

data Triple = Triple Int32 Text Int32
         deriving (Generic, Show)

instance FromRow Triple

unit_regression_text :: IO ()
unit_regression_text = do
  let conInfo = testConnectInfo
  con <- connect conInfo
  let t = "large world of text which is like really huge"
  res <- query con "select * from Chinook.dbo.[MichelinCustomerProfile] order by [CUSTOMER_NUMBER]" :: IO (Vector (Identity Text))
  disconnect con
  pure ()

unit_timeOfDay :: IO ()
unit_timeOfDay = do
  let conInfo = testConnectInfo
  con <- connect conInfo
  --let -- s = "'00:00:00'"  
      -- t1 = read s :: TimeOfDay
      -- "select CAST ('" <> (T.pack $ show t1) <> "' AS TIME)
  (query con ("select CAST ('10:15:10.56855412' AS TIME)") :: IO (Vector (Identity TimeOfDay))) >>= print    
  (query con ("select CAST ('00:00:10.5412' AS TIME)") :: IO (Vector (Identity TimeOfDay))) >>= print
  (query con ("select CAST ('00:00:00' AS TIME)") :: IO (Vector (Identity TimeOfDay))) >>= print
  
  disconnect con
  pure ()



_unit_connect :: IO ()
_unit_connect = do
  let conInfo = testConnectInfo
  con <- connect conInfo
  res <- query con "select * from Album" :: IO ((Vector Album))
  res1 <- query con "select img from test" :: IO (Vector (Identity Image))

  print res
  print (length res1)
  mapM (BSL.writeFile "/tmp/output" . getImage . runIdentity) res1 >> pure ()
  disconnect con
  pure ()

_unit_sqlinsert :: IO ()
_unit_sqlinsert = do
  let conInfo = testConnectInfo
  con <- connect conInfo  
  res <- execute con "insert test1 (test_id, col1, col2, col3, col4, smallint, bit, tinyint, bigint, dbl, flt, datec, tod, dt, dt2, sdt, dtz, utc1, uuid, ntxt, char10, nchar10) VALUES (5, '12/13/2012', 3, 'fdfd', 'fdffddf', 32, 1, 23,99999999,4, 6.5,'1/13/2013', '00:00:00', '2015-03-19 05:15:18.123', '2015-03-19 05:15:18.123', '2015-03-19 05:15:18.123', '2015-03-19 05:15:18.123+05:30', '2015-03-19 05:15:18.123+05:30', '0E984725-C51C-4BF4-9960-E1C80E27ABA0', N'🌀', 'dfd', N'🌀');"
  print res

test_roundTrip1 :: TestTree
test_roundTrip1 =
  withResource (connect testConnectInfo)
               disconnect $ \r -> 
  testGroup "round trip tests 1"
  [ testProperty "maxBound @Int" $ withTests 100 $ roundTrip r (Gen.int $ Range.singleton $ maxBound @Int)
  , testProperty "minBound @Int" $ withTests 100 $ roundTrip r (Gen.int $ Range.singleton $ minBound @Int)
  
  -- , testProperty "maxBound @Int8" $ withTests 1 $ roundTrip r (Gen.int8 $ Range.singleton $ maxBound @Int8)
  -- , testProperty "minBound @Int8" $ withTests 1 $ roundTrip r (Gen.int8 $ Range.singleton $ minBound @Int8)

  , testProperty "maxBound @Int16" $ withTests 100 $ roundTrip r (Gen.int16 $ Range.singleton $ maxBound @Int16)
  , testProperty "minBound @Int16" $ withTests 100 $ roundTrip r (Gen.int16 $ Range.singleton $ minBound @Int16)

  , testProperty "maxBound @Int32" $ withTests 100 $ roundTrip r (Gen.int32 $ Range.singleton $ maxBound @Int32)
  , testProperty "minBound @Int32" $ withTests 100 $ roundTrip r (Gen.int32 $ Range.singleton $ minBound @Int32)

  , testProperty "maxBound @Int64" $ withTests 100 $ roundTrip r (Gen.int64 $ Range.singleton $ maxBound @Int64)
  , testProperty "minBound @Int64" $ withTests 100 $ roundTrip r (Gen.int64 $ Range.singleton $ minBound @Int64)

  -- , testProperty "minBound @Money" $ withTests 100 $ roundTrip r (toMoney <$> (Gen.int64 $ Range.singleton $ minBound @Int64))  
  -- , testProperty "maxBound @Money" $ withTests 100 $ roundTrip r (toMoney <$> (Gen.int64 $ Range.singleton $ maxBound @Int64))  

  -- , testProperty "minBound @SmallMoney" $ withTests 100 $ roundTrip r (toSmallMoney <$> (Gen.int32 $ Range.singleton $ minBound @Int32))  -- OK
  -- , testProperty "maxBound @SmallMoney" $ withTests 100 $ roundTrip r (toSmallMoney <$> (Gen.int32 $ Range.singleton $ maxBound @Int32))  -- OK
  
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
  , testProperty "@TimeOfDay" $ withTests 100 $ roundTrip r timeOfDay
  , testProperty "@LocalTime" $ withTests 100 $ roundTrip r localTime
  , testProperty "@ZonedTime" $ withTests 100 $ roundTripWith r zonedTime ppZonedTime
  
  , testProperty "@Float" $ withTests 100 $ roundTrip r (Gen.float $ Range.exponentialFloat (-100) 100) -- OK
  , testProperty "@Double" $ withTests 100 $ roundTrip r (Gen.double $ Range.exponentialFloat (-100) 100) -- OK
  , testProperty "@Maybe Double" $ withTests 100 $ roundTripWith r (Gen.maybe $ Gen.double $ Range.exponentialFloat (-100) 100) ppMaybe -- OK
  
  , testProperty "@ASCIIText" $ withTests 100 $ roundTripWith r (asciiText (Range.linear 0 1000)) (\t -> "'" <> T.replace "'" "''" (getASCIIText t) <> "'")
  , testProperty "@Bytestring" $ withTests 50 $ roundTripWith r byteGen (\t -> "0x" <> T.pack (B.foldr showHex "" t))
  , testProperty "@Text" $ withTests 100 $ roundTripWith r (Gen.text (Range.linear 0 1000) (Gen.filter (\x -> ord x > 40 && x /= '\'') Gen.unicode)) (\t -> "N'" <> t <> "'")
  , testProperty "@Maybe ASCIIText" $ withTests 100 $ roundTripWith r (Gen.maybe $ asciiText (Range.linear 0 1000)) (ppMaybeWith (\t -> "'" <> T.replace "'" "''" (getASCIIText t) <> "'"))
  , testProperty "@Maybe Bytestring" $ withTests 50 $ roundTripWith r (Gen.maybe $ byteGen) (ppMaybeWith (\t -> "0x" <> T.pack (B.foldr showHex "" t)))
  , testProperty "@Maybe Text" $ withTests 100 $ roundTripWith r (Gen.maybe (Gen.text (Range.linear 0 1000) (Gen.filter (\x -> ord x > 40 && x /= '\'') Gen.unicode))) (ppMaybeWith (\t -> "N'" <> t <> "'"))
  
  ]

  where toMoney i64 = Money (read (show i64) / 10000)
        toSmallMoney i32 = SmallMoney (read (show i32) / 10000)
        byteGen = db_bytes (Range.linear 0 1000)
        doubleNull = BS.pack [0x0, 0x0]
        ppMaybe = ppMaybeWith (T.pack . show)
        ppMaybeWith f a = case a of
          Nothing -> "null"
          Just v  -> f v
                      
        ppZonedTime (ZonedTime lt tz) =
          T.pack $ show lt <> " " <> ppTz tz


test_roundTrip2 :: TestTree
test_roundTrip2 =
  withResource (connect testConnectInfo)
               disconnect $ \r -> 
  testGroup "round trip tests 2"
  [ testProperty "@SizedText 50" $ withTests 100 $ roundTripWith r (sized @50 <$> Gen.text (Range.singleton 25) (Gen.filter (\x -> ord x > 40 && x /= '\'') Gen.unicode)) (\t -> "N'" <> getSized t <> "'")
  , testProperty "@SizedText 50" $ withTests 100 $ roundTripWith r (sized @50 <$> Gen.text (Range.singleton 50) (Gen.filter (\x -> ord x > 40 && x /= '\'' && ord x < 65536) Gen.unicode)) (\t -> "N'" <> getSized t <> "'")
  , testProperty "@SizedASCIIText 50" $ withTests 100 $ roundTripWith r (sized @50 <$> asciiText (Range.singleton 50)) (\t -> "'" <> T.replace "'" "''" (getASCIIText (getSized t)) <> "'")
  , testProperty "@SizedByteString 50" $ withTests 100 $ roundTripWith r byteGen (\t -> "0x" <> T.pack (B.foldr showHex "" (getSized t)))
  ]

  where sized :: forall n a. a -> Sized n a
        sized = Sized
        byteGen = sized @50 <$> db_bytes (Range.singleton 50)


db_bytes :: MonadGen m => Range Int -> m ByteString
db_bytes range =
  fmap B.pack $
  Gen.choice
  [
      Gen.list range . Gen.word8 $
        Range.constant
          (fromIntegral $ ord 'a')
          (fromIntegral $ ord 'z')

    , Gen.list range . Gen.word8 $
        Range.constant 30 maxBound
    ]
