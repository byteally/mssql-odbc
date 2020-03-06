{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Database.MSSQL.Internal.SQLTypes where

import Foreign.C.Types
import Foreign.Storable
import Data.Time

#ifdef mingw32_HOST_OS
C.include "<windows.h>"
#endif
  
#include <sqltypes.h>
#include <ss.h>

newtype CDate = CDate{ getDate :: Day }
              deriving (Show, Eq)
                

instance Storable CDate where
  alignment _ = #{alignment SQL_DATE_STRUCT}
  sizeOf _ = #{size SQL_DATE_STRUCT}
  peek ptr = do
    yr <- #{peek SQL_DATE_STRUCT, year} ptr :: IO CShort
    mon <- #{peek SQL_DATE_STRUCT, month} ptr :: IO CShort
    dy <- #{peek SQL_DATE_STRUCT, day} ptr :: IO CShort
    pure $ CDate (fromGregorian (toInteger yr) (fromIntegral mon) (fromIntegral dy))
  poke ptr dt = do
    let (yr, mon, dy) = toGregorian $ getDate dt
    #{poke SQL_DATE_STRUCT, year} ptr (fromIntegral yr :: CShort)
    #{poke SQL_DATE_STRUCT, month} ptr (fromIntegral mon :: CShort)
    #{poke SQL_DATE_STRUCT, day} ptr (fromIntegral dy :: CShort)


newtype CTimeOfDay = CTimeOfDay {getTimeOfDay :: TimeOfDay}
                   deriving (Show, Eq)


{-
instance Storable CTimeOfDay where
  alignment _ = #{alignment SQL_TIME_STRUCT}
  sizeOf _ = #{size SQL_TIME_STRUCT}
  peek ptr = do
    hour <- #{peek SQL_TIME_STRUCT, hour} ptr :: IO CUShort
    minute <- #{peek SQL_TIME_STRUCT, minute} ptr :: IO CUShort
    second <- #{peek SQL_TIME_STRUCT, second} ptr :: IO CUShort
    pure $ CTimeOfDay (TimeOfDay
                       { todHour = fromIntegral hour
                       , todMin = fromIntegral minute
                       , todSec = fromIntegral second
                       })
  poke ptr (CTimeOfDay tod) = do
    #{poke SQL_TIME_STRUCT, hour} ptr (fromIntegral (todHour tod) :: CUShort)
    #{poke SQL_TIME_STRUCT, minute} ptr (fromIntegral (todMin tod) :: CUShort)
    #{poke SQL_TIME_STRUCT, second} ptr (round (todSec tod) :: CUShort) -- TODO: Fix clipping
-}

instance Storable CTimeOfDay where
  alignment _ = #{alignment SQL_SS_TIME2_STRUCT}
  sizeOf _ = #{size SQL_SS_TIME2_STRUCT}
  peek ptr = do
    hour <- #{peek SQL_SS_TIME2_STRUCT, hour} ptr :: IO CUShort
    minute <- #{peek SQL_SS_TIME2_STRUCT, minute} ptr :: IO CUShort
    second <- #{peek SQL_SS_TIME2_STRUCT, second} ptr :: IO CUShort
    nanosecond <- #{peek SQL_SS_TIME2_STRUCT, fraction} ptr :: IO CUInt
    pure $ CTimeOfDay (TimeOfDay
                       { todHour = fromIntegral hour
                       , todMin = fromIntegral minute
                       , todSec = fromIntegral second + (fromIntegral nanosecond / 1000000000)
                       })
  poke ptr (CTimeOfDay tod) = do
    #{poke SQL_SS_TIME2_STRUCT, hour} ptr (fromIntegral (todHour tod) :: CUShort)
    #{poke SQL_SS_TIME2_STRUCT, minute} ptr (fromIntegral (todMin tod) :: CUShort)
    #{poke SQL_SS_TIME2_STRUCT, second} ptr (round (todSec tod) :: CUShort) -- TODO: Fix clipping

newtype CLocalTime = CLocalTime { getLocalTime :: LocalTime }
                   deriving (Show, Eq)
                                  
instance Storable CLocalTime where
  alignment _ = #{alignment SQL_TIMESTAMP_STRUCT}
  sizeOf _ = #{size SQL_TIMESTAMP_STRUCT}
  peek ptr = do
    yr <- #{peek SQL_TIMESTAMP_STRUCT, year} ptr :: IO CShort
    mon <- #{peek SQL_TIMESTAMP_STRUCT, month} ptr :: IO CUShort
    dy <- #{peek SQL_TIMESTAMP_STRUCT, day} ptr :: IO CUShort
    hour <- #{peek SQL_TIMESTAMP_STRUCT, hour} ptr :: IO CUShort
    minute <- #{peek SQL_TIMESTAMP_STRUCT, minute} ptr :: IO CUShort
    second <- #{peek SQL_TIMESTAMP_STRUCT, second} ptr :: IO CUShort
    nanosecond <- #{peek SQL_TIMESTAMP_STRUCT, fraction} ptr :: IO CUInt
    pure $ CLocalTime (LocalTime
                       { localDay = (fromGregorian (toInteger yr) (fromIntegral mon) (fromIntegral dy))
                       , localTimeOfDay = TimeOfDay
                                          { todHour = fromIntegral hour
                                          , todMin = fromIntegral minute
                                          , todSec = fromIntegral second + (fromIntegral nanosecond / 1000000000)
                                          }
                       })
  poke ptr (CLocalTime lt) = do
    let (yr, mon, dy) = toGregorian $ localDay lt
    #{poke SQL_TIMESTAMP_STRUCT, year} ptr (fromIntegral yr :: CShort)
    #{poke SQL_TIMESTAMP_STRUCT, month} ptr (fromIntegral mon :: CUShort)
    #{poke SQL_TIMESTAMP_STRUCT, day} ptr (fromIntegral dy :: CUShort)
    let tod = localTimeOfDay lt
    #{poke SQL_TIMESTAMP_STRUCT, hour} ptr (fromIntegral (todHour tod) :: CUShort)
    #{poke SQL_TIMESTAMP_STRUCT, minute} ptr (fromIntegral (todMin tod) :: CUShort)
    #{poke SQL_TIMESTAMP_STRUCT, second} ptr (round (todSec tod) :: CUShort) -- TODO: Fix clipping
    #{poke SQL_TIMESTAMP_STRUCT, fraction} ptr (0 :: CUInt) -- TODO: Fix


newtype CZonedTime = CZonedTime { getZonedTime :: ZonedTime }
                   deriving (Show)


instance Storable CZonedTime where
  alignment _ = #{alignment SQL_SS_TIMESTAMPOFFSET_STRUCT}
  sizeOf _ = #{size SQL_SS_TIMESTAMPOFFSET_STRUCT}
  peek ptr = do
    yr <- #{peek SQL_SS_TIMESTAMPOFFSET_STRUCT, year} ptr :: IO CShort
    mon <- #{peek SQL_SS_TIMESTAMPOFFSET_STRUCT, month} ptr :: IO CUShort
    dy <- #{peek SQL_SS_TIMESTAMPOFFSET_STRUCT, day} ptr :: IO CUShort
    hour <- #{peek SQL_SS_TIMESTAMPOFFSET_STRUCT, hour} ptr :: IO CUShort
    minute <- #{peek SQL_SS_TIMESTAMPOFFSET_STRUCT, minute} ptr :: IO CUShort
    second <- #{peek SQL_SS_TIMESTAMPOFFSET_STRUCT, second} ptr :: IO CUShort
    nanosecond <- #{peek SQL_SS_TIMESTAMPOFFSET_STRUCT, fraction} ptr :: IO CUInt
    tzHour <- #{peek SQL_SS_TIMESTAMPOFFSET_STRUCT, timezone_hour} ptr :: IO CShort
    tzMinute <- #{peek SQL_SS_TIMESTAMPOFFSET_STRUCT, timezone_minute} ptr :: IO CShort
    pure $ CZonedTime $ ZonedTime
      { zonedTimeToLocalTime= LocalTime
                              { localDay = (fromGregorian (toInteger yr) (fromIntegral mon) (fromIntegral dy))
                              , localTimeOfDay = TimeOfDay
                                                 { todHour = fromIntegral hour
                                                 , todMin = fromIntegral minute
                                                 , todSec = fromIntegral second + (fromIntegral nanosecond / 1000000000)
                                                 }
                              }
      , zonedTimeZone = minutesToTimeZone $ fromIntegral ((tzHour * 60) + tzMinute)
      }
  poke ptr (CZonedTime (ZonedTime lt tz)) = do
    let (yr, mon, dy) = toGregorian $ localDay lt
    #{poke SQL_SS_TIMESTAMPOFFSET_STRUCT, year} ptr (fromIntegral yr :: CShort)
    #{poke SQL_SS_TIMESTAMPOFFSET_STRUCT, month} ptr (fromIntegral mon :: CUShort)
    #{poke SQL_SS_TIMESTAMPOFFSET_STRUCT, day} ptr (fromIntegral dy :: CUShort)
    let
      tod = localTimeOfDay lt
      (h,m) = timeZoneMinutes tz `divMod` 60
    
    #{poke SQL_SS_TIMESTAMPOFFSET_STRUCT, hour} ptr (fromIntegral (todHour tod) :: CUShort)
    #{poke SQL_SS_TIMESTAMPOFFSET_STRUCT, minute} ptr (fromIntegral (todMin tod) :: CUShort)
    #{poke SQL_SS_TIMESTAMPOFFSET_STRUCT, second} ptr (round (todSec tod) :: CUShort) -- TODO: Fix clipping
    #{poke SQL_SS_TIMESTAMPOFFSET_STRUCT, fraction} ptr (0 :: CUInt) -- TODO: Fix
    #{poke SQL_SS_TIMESTAMPOFFSET_STRUCT, timezone_hour} ptr (fromIntegral h :: CUShort) -- TODO: Fix
    #{poke SQL_SS_TIMESTAMPOFFSET_STRUCT, timezone_minute} ptr (fromIntegral m :: CUShort) -- TODO: Fix
