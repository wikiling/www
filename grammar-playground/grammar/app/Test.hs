{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Test where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.Set
import Data.Text
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

--
-- Complete the 'miniMaxSum' function below.
--
-- The function accepts INTEGER_ARRAY arr as parameter.
--

miniMaxSum :: [Int] -> [Int]
miniMaxSum arr = do
    let tot = sum arr
    let choices = Data.List.map (`subtract` tot) arr
    [Data.List.minimum choices, Data.List.maximum choices]

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do

    arrTemp <- getLine

    let arr = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip arrTemp

    print $ Data.List.intercalate " " $ Data.List.map show $ miniMaxSum arr
