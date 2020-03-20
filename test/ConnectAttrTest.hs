{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns               #-}

module ConnectAttrTest where

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

test_connectAttr :: TestTree
test_connectAttr =
  testGroup "Connect attribute tests"
  [ -- testProperty "binary" $ withTests 100 $ binaryProp r
    -- testProperty "text" $ withTests 1 $ textProp r
    -- testProperty "before attrs" $ withTests 100 $ beforeAttrProp r
    -- testProperty "after attrs" $ withTests 100 $ afterAttrProp r  
  ]

{-
afterAttrProp = beforeAttrProp

beforeAttrProp r =
  property $ do
    val <- forAll undefined
    sqlSetConnectAttr (_hdbc r) val
-}                               
