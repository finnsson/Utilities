{-# OPTIONS_GHC -fglasgow-exts -XTemplateHaskell #-}
module Utilities.Test.QuickQueryALTest where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import Test.HUnit

import Data.List
import Data.Char
import Database.HDBC
import Database.HDBC.PostgreSQL
import Maybe

import qualified Control.Exception as E

import Utilities.HDBC

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import TestGenerator


main = defaultMain [quickQueryALTest] 

quickQueryALTest = $testGroupGenerator

-- Fixture
cs = "dbname=QuickQueryALTest user=test password=test"

testQuickQueryAL =
  do let sql = "select * from \"int_int\";"
         expected = [[("fst",SqlInteger 11),("snd",SqlInteger 22)]]
     conn <- connectPostgreSQL cs
     actual <- quickQueryAL conn sql []
     expected @=? actual
