{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns               #-}

module SQLBindColTest where

import Utils
import Test.Tasty
import Database.MSSQL
import qualified Data.ByteString as B
import qualified Data.Text as T
import GHC.Generics
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.Coerce
import Numeric (showHex)
import Test.Tasty.Hedgehog
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Data.Typeable
import Foreign.C.Types
import qualified Foreign.C.String as F
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Data.Int
import Data.Word
import Data.Time
import Control.Monad.IO.Class
import Data.Char (ord)

test_bindColUnboundRoundTrip :: TestTree
test_bindColUnboundRoundTrip =
  withResource (connect testConnectInfo)
                disconnect $ \r -> 
  testGroup "SQLBindcol unbound round trip tests"
  [ -- testProperty "binary" $ withTests 100 $ binaryProp r
    -- testProperty "text" $ withTests 1 $ textProp r
  ]

_test_bindColBoundRoundTrip :: TestTree
_test_bindColBoundRoundTrip =
  withResource (connect testConnectInfo)
                disconnect $ \r -> 
  testGroup "SQLBindcol bound round trip tests"
  [ testProperty "int"   $ withTests 100 $ intProp r
  , testProperty "int16" $ withTests 100 $ int16Prop r
  , testProperty "int32" $ withTests 100 $ int32Prop r
  , testProperty "int64" $ withTests 100 $ int64Prop r
  , testProperty "word8" $ withTests 100 $ word8Prop r
  , testProperty "bool"  $ withTests 100 $ boolProp r
  , testProperty "day"   $ withTests 100 $ dayProp r      
  , testProperty "timeOfDay"  $ withTests 100 $ timeOfDayProp r
  , testProperty "localTimeProp" $ withTests 100 $ localTimeProp r
  , testProperty "zonedTimeProp" $ withTests 100 $ zonedTimeProp r  
  , testProperty "floatProp" $ withTests 100 $ floatProp r
  , testProperty "doubleProp" $ withTests 100 $ doubleProp r
  ]

-- NOTE: 0x0 was failing
-- NOTE: on larger tests binaryProp is hanging - is it due to huge allocations?. NOTE: when filter was removed, got quick failure.
binaryProp con = -- property $ do
  tripTwice con byteGen ppBytes SQLBound (ppBytes . getSQLBound)
  -- let txt = "\NUL\NUL" :: LBS.ByteString
  -- res <- do
  --   q <- mkQuery ppBytes txt
  --   liftIO $ print q
  --   evalQuery con q
  -- pure txt === res

  where byteGen = Gen.bytes (Range.linear 0 8000)
        ppBytes t = "0x" <> T.pack (B.foldr showHex "" t)

textProp con = property $ do
  txt <- forAll textGen
  -- liftIO $ putStrLn $ "Text is: " <> T.unpack txt
  !res <- do
    q <- mkQuery quote txt
    -- liftIO $ putStrLn $ "Query is: " <> T.unpack q
    evalQuery con q
  pure txt === res
  
  -- tripTwice con textGen quote SQLBound (quote . getSQLBound)
  where textGen = Gen.text (Range.linear 1 10) (Gen.filter (\x -> ord x > 40) Gen.ascii)
        quote t = "'" <> T.replace "'" "''" t <> "'"

intProp con =
  tripTwice con intGen tshow SQLUnbound (tshow . getSQLUnbound)  
  where intGen = Gen.int (Range.linear minBound maxBound)

int16Prop con =
  tripTwice con intGen tshow SQLUnbound (tshow . getSQLUnbound)  
  where intGen = Gen.int16 (Range.linear minBound maxBound)

int32Prop con =
  tripTwice con intGen tshow SQLUnbound (tshow . getSQLUnbound)  
  where intGen = Gen.int32 (Range.linear minBound maxBound)

int64Prop con =
  tripTwice con intGen tshow SQLUnbound (tshow . getSQLUnbound)  
  where intGen = Gen.int64 (Range.linear minBound maxBound)

word8Prop con =
  tripTwice con intGen tshow SQLUnbound (tshow . getSQLUnbound)  
  where intGen = Gen.word8 (Range.linear minBound maxBound)

boolProp con =
  tripTwice con boolGen bshow SQLUnbound (bshow . getSQLUnbound)  
  where boolGen = Gen.bool
        bshow True = "1"
        bshow False = "0"

timeOfDayProp con =
  tripTwice con timeOfDayGen tshow SQLUnbound (tshow . getSQLUnbound)  
  where timeOfDayGen = timeOfDay

dayProp con =
  tripTwice con dayGen tshow SQLUnbound (tshow . getSQLUnbound)  
  where dayGen = day

localTimeProp con =
  tripTwice con localTimeGen tshow SQLUnbound (tshow . getSQLUnbound)  
  where localTimeGen = localTime

zonedTimeProp con =
  tripTwice con zonedTimeGen ppZonedTime SQLUnbound (ppZonedTime . getSQLUnbound)  
  where zonedTimeGen = zonedTime
        ppZonedTime (ZonedTime lt tz) =
          T.pack $ show lt <> " " <> ppTz tz

floatProp con =
  tripTwice con floatGen tshow SQLUnbound (tshow . getSQLUnbound)  
  where floatGen = Gen.float (Range.exponentialFloat (-100) 100)

doubleProp con =
  tripTwice con doubleGen tshow SQLUnbound (tshow . getSQLUnbound)  
  where doubleGen = Gen.double (Range.exponentialFloat (-100) 100)

tripTwice :: (FromField a, FromField b, Typeable b, Typeable a, Show a, Show b, Eq a, Eq b) => IO Connection -> Gen a -> (a -> Text) -> (a -> b) -> (b -> Text) -> Property
tripTwice con gen at fab bt = do
  property $ do
    val <- forAll gen
    trippingM val (mkQuery at) (evalQuery con)
    trippingM (fab val) (mkQuery bt) (evalQuery con)
  
-- NOTE: instances for testing
newtype SQLBound a = SQLBound { getSQLBound :: a }
                   deriving (Show, Generic, Eq)

newtype SQLUnbound a = SQLUnbound { getSQLUnbound :: a }
                     deriving (Show, Generic, Eq, Num)

instance FromField (SQLBound B.ByteString) where
  type FieldBufferType (SQLBound B.ByteString) = CBinary  
  fromField = \i -> case (getColBuffer i) of
    ColBindBuffer byteCountFP bytesFP -> do
      byteCount <- peekFP byteCountFP
      pure (SQLBound $ B.fromForeignPtr (coerce bytesFP) 0 (fromIntegral byteCount))

instance FromField (SQLBound T.Text) where
  type FieldBufferType (SQLBound T.Text) = CWchar
  fromField = \i -> case (getColBuffer i) of
    ColBindBuffer charCountFP textFP -> do
      charCount <- peekFP charCountFP
      a <- withForeignPtr textFP $ \cwcharP -> F.peekCWStringLen (coerce cwcharP, fromIntegral charCount)
      putStrLn $ "charCount : " ++ show (a, charCount)
      pure (SQLBound (T.pack a))

{-      
    \byteCount bytesP -> do
      bs <- B.packCStringLen (coerce bytesP, fromIntegral byteCount)
      putStrLn $ "byteCount : " ++ show (bs, byteCount)
      pure (SQLBound bs)
-}

instance FromField (SQLUnbound Int) where
  type FieldBufferType (SQLUnbound Int) = CGetDataBound CBigInt
  fromField = \i -> boundWith (getColBuffer i) (\_ -> fmap fromIntegral . peek)

instance FromField (SQLUnbound Int16) where
  type FieldBufferType (SQLUnbound Int16) = CGetDataBound CSmallInt 
  fromField = \i -> boundWith (getColBuffer i) (\_ -> fmap fromIntegral . peek)

instance FromField (SQLUnbound Int32) where
  type FieldBufferType (SQLUnbound Int32) = CGetDataBound CLong 
  fromField = \i -> boundWith (getColBuffer i) (\_ -> fmap fromIntegral . peek)

instance FromField (SQLUnbound Int64) where
  type FieldBufferType (SQLUnbound Int64) = CGetDataBound CBigInt
  fromField = \i -> boundWith (getColBuffer i) (\_ -> fmap fromIntegral . peek)

instance FromField (SQLUnbound Word8) where
  type FieldBufferType (SQLUnbound Word8) = CGetDataBound CUTinyInt
  fromField = \i -> boundWith (getColBuffer i) (\_ -> fmap fromIntegral . peek)

instance FromField (SQLUnbound Bool) where
  type FieldBufferType (SQLUnbound Bool) = CGetDataBound CBool
  fromField = \i -> boundWith (getColBuffer i) (\_ a -> do
                                                  v <- peek a
                                                  pure $ SQLUnbound $ if v == 1 then True else False)

instance FromField (SQLUnbound Day) where
  type FieldBufferType (SQLUnbound Day) = CGetDataBound CDate
  fromField = \i -> boundWith (getColBuffer i) (\_ -> fmap (SQLUnbound . getDate . coerce) . peek)

instance FromField (SQLUnbound TimeOfDay) where
  type FieldBufferType (SQLUnbound TimeOfDay) = CGetDataBound CTimeOfDay
  fromField = \i -> boundWith (getColBuffer i) (\_ -> fmap (SQLUnbound . getTimeOfDay . coerce) . peek)

instance FromField (SQLUnbound LocalTime) where
  type FieldBufferType (SQLUnbound LocalTime) = CGetDataBound CLocalTime
  fromField = \i -> boundWith (getColBuffer i) (\_ -> fmap (SQLUnbound . getLocalTime . coerce) . peek)

instance FromField (SQLUnbound Float) where
  type FieldBufferType (SQLUnbound Float) = CGetDataBound CFloat
  fromField = \i -> boundWith (getColBuffer i) (\_ -> fmap (SQLUnbound . coerce) . peek)

instance FromField (SQLUnbound Double) where
  type FieldBufferType (SQLUnbound Double) = CGetDataBound CDouble
  fromField = \i -> boundWith (getColBuffer i) (\_ -> fmap (SQLUnbound . coerce) . peek)

instance FromField (SQLUnbound ZonedTime) where
  type FieldBufferType (SQLUnbound ZonedTime) = CGetDataBound CZonedTime
  fromField = \i -> boundWith (getColBuffer i) (\_ -> fmap (SQLUnbound . Database.MSSQL.getZonedTime . coerce) . peek)

tshow :: (Show a) => a -> Text
tshow = T.pack . show
