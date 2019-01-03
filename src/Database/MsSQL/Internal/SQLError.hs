module Database.MsSQL.Internal.SQLError where

import Data.Text (Text)
import Data.Int


data SQLError = SQLError
  { sqlState   :: Text
  , sqlMessage :: Text
  , sqlReturn  :: Int32
  } deriving (Show, Eq)

type SQLErrors = [SQLError]
