{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}

module Utils where

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty.Hedgehog
import Data.Functor.Identity
import Data.Time
import Database.MSSQL
import Control.Exception
import Data.Text
import qualified Data.Text as T
import Data.Functor.Compose
import Data.Vector
import Data.Typeable
import qualified Data.HashMap.Strict as HM
import qualified Data.String as S
import Control.Monad.IO.Class
import qualified Data.Text.IO as T

ppTz :: TimeZone -> String
ppTz (TimeZone tzms _ _) =
  let hrs = posTzms `div` 60
      mins = posTzms `mod` 60

      
      (sign, posTzms) = if tzms < 0
                        then (False, (-1) * tzms)
                        else (True, tzms)
      ppSign True  = "+"
      ppSign False = "-"
  in  ppSign sign <> show hrs <> ":" <> if mins < 10 then "0" <> show mins else show mins
        
asciiText :: MonadGen m => Range Int -> m ASCIIText
asciiText r =
  (ASCIIText . T.pack) <$> Gen.list r (Gen.enum '\40' '\127')

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

timeZone :: ( MonadGen m
             , GenBase m ~ Identity
             ) => m TimeZone
timeZone = do
  tzms <- Gen.enum (-720) 720
  sOnly <- Gen.bool
  tzn <- Gen.element ["UTC", "UT", "GMT", "EST", "EDT", "CST", "CDT", "MST", "MDT", "PST", "PDT"]
  pure (TimeZone tzms sOnly tzn)
  

zonedTime :: ( MonadGen m
             , GenBase m ~ Identity
             ) => m ZonedTime
zonedTime = ZonedTime <$> localTime <*> timeZone

roundTrip :: forall a.
  ( Show a
  , Eq a
  , FromField a
  , Typeable a
  ) => IO Connection -> Gen a -> Property
roundTrip con gen =
  roundTripWith con gen (T.pack . show)

roundTripWith :: forall a.
  ( Eq a
  , FromField a
  , Typeable a
  , Show a
  ) => IO Connection -> Gen a -> (a -> Text) -> Property
roundTripWith con gen f =  
  property $ do
    val <- forAll gen
    evalIO $ T.putStrLn $ "<Value :: > " <> (f val)
    trippingM val (mkQuery f) (evalQuery con)
    pure ()

mkQuery f a =
  let fmtedVal =
        if isQuoted
           then "'" <> f a <> "'"
           else f a 
      (sqlType, isQuoted) = getSQLType a
  in eval $ S.fromString (T.unpack ("select CAST (" <> fmtedVal <> " AS " <> sqlType <> ")"))

evalQuery con q = do
  evalIO . fmap wrapCompose $ do
      con' <- con
      T.putStrLn $ "Query is: " <> q
      res <- query con' q
      putStrLn $ "Done: " <> show res
      return res
      

wrapCompose :: forall a b.
              Vector (Identity b) ->
              Compose Vector Identity b
wrapCompose = Compose

data EvalException = EvalException Text
                   deriving (Show)

instance Exception EvalException                   
          
getSQLType :: Typeable a => a -> (Text, Bool)
getSQLType a = case show $ typeOf a of
  "Int"    -> ("BIGINT", False)
  "SQLUnbound Int"    -> ("BIGINT", False)  
  "Int16"  -> ("SMALLINT", False)
  "SQLUnbound Int16"  -> ("SMALLINT", False)  
  "Int32"  -> ("INTEGER", False)
  "SQLUnbound Int32"  -> ("INTEGER", False)  
  "Int64"  -> ("BIGINT", False)
  "SQLUnbound Int64"  -> ("BIGINT", False)  
  "Word8"  -> ("TINYINT", False)
  "SQLUnbound Word8"  -> ("TINYINT", False)  
  "Bool"   -> ("BIT", False)
  "SQLUnbound Bool"   -> ("BIT", False)  
  "Day"    -> ("DATE", True)
  "SQLUnbound Day"    -> ("DATE", True)  
  "TimeOfDay" -> ("TIME", True)
  "SQLUnbound TimeOfDay" -> ("TIME", True)  
  "LocalTime" -> ("DATETIME2", True)
  "SQLUnbound LocalTime" -> ("DATETIME2", True)  
  "Float"     -> ("REAL", False) 
  "SQLUnbound Float" -> ("REAL", False) 
  "Double"    -> ("FLOAT(53)", False)
  "SQLUnbound Double"    -> ("FLOAT(53)", False)  
  "Money"    -> ("MONEY", True)
  "SmallMoney" -> ("SMALLMONEY", True)  
  "ASCIIText" -> ("VARCHAR(5000)", False)
  "Text" -> ("NTEXT", False)
  "SQLBound Text" -> ("NTEXT", False)  
  "ByteString" -> ("VARBINARY(8000)", False)
  "SQLBound ByteString" -> ("VARBINARY(8000)", False)  
  "Maybe Double" -> ("FLOAT(53)", False)
  "ZonedTime" -> ("datetimeoffset", True)
  "SQLUnbound ZonedTime" -> ("datetimeoffset", True)  
  "Sized 97 Text" -> ("NVARCHAR(97)", False)  
  "Sized 97 ASCIIText" -> ("VARCHAR(97)", False)
  e -> error $ "Panic: " <> show e

  
{-
  "Int8"   -> "TINYINT"
  "Word"   -> "NUMERIC(20,0)"
  "Word16" -> "NUMERIC(5,0)"
  "Word32" -> "NUMERIC(10,0)"
  "Word64" -> "NUMERIC(20,0)"
-}

-- NOTE: orphan instance for equality
instance Eq ZonedTime where
  (ZonedTime t1 tz1) == (ZonedTime t2 tz2) = t1 == t2 && ppTz tz1 == ppTz tz2

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

locConnStr :: ConnectionString
locConnStr = ConnectionString
  { database          = "returns"
  , server            = "slstwinfordb01.ad.sunlandls.com"
                      -- "10.10.0.33" --"slstwinfordb01.ad.sunlandls.com"
  , port              = 1433
  , user              = "sunlandls"
  , password          = "sunland.wms~123"
  , odbcDriver        = odbcSQLServer17
  , connectProperties = mempty
  }  


testConnectInfo :: ConnectInfo
testConnectInfo = connectInfo localConnectionStr -- locConnStr
-- CAST (-99.99999999999979 AS FLOAT(53))

