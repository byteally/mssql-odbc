{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP                        #-}
module Database.MSSQL.Internal.SQLError where

import Data.Text (Text)
import Data.Int
import Control.Exception
#if __GLASGOW_HASKELL__ < 802
import Data.Semigroup
#endif

data SQLError = SQLError
  { sqlState   :: Text
  , sqlMessage :: Text
  , sqlReturn  :: Int32
  } deriving (Show, Eq)

instance Exception SQLError 

newtype SQLErrors = SQLErrors { getSqlErrors :: [SQLError] }
                  deriving (Show, Eq, Semigroup, Monoid)

instance Exception SQLErrors

