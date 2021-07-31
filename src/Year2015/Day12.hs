{-# LANGUAGE OverloadedStrings #-}

module Year2015.Day12 where

import Data.Aeson (Object, Value)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashMap.Strict as M
import Data.Maybe (fromJust)
import qualified Data.Scientific as S
import qualified Data.Vector as V
import qualified Util as U

input :: IO String
input = U.getData "data/2015/Day12.txt"

total :: Value -> Double
total (A.Array es) = sum $ map total $ V.toList es
total (A.Object m) = sum $ map total $ M.elems m
total (A.String _) = 0
total (A.Number x) = S.toRealFloat x
total (A.Bool _) = 0
total A.Null = 0

totalNoRed :: Value -> Double
totalNoRed (A.Array es) = sum $ map totalNoRed $ V.toList es
totalNoRed (A.Object m) = let es = M.elems m in if A.String "red" `elem` es then 0 else sum $ map totalNoRed es
totalNoRed (A.String _) = 0
totalNoRed (A.Number x) = S.toRealFloat x
totalNoRed (A.Bool _) = 0
totalNoRed A.Null = 0

y2015d12ex1 :: IO ()
y2015d12ex1 = do
  contents <- input
  let ans = round $ total $ fromJust $ A.decode $ BS.pack contents
  print ans

y2015d12ex2 :: IO ()
y2015d12ex2 = do
  contents <- input
  let ans = round $ totalNoRed $ fromJust $ A.decode $ BS.pack contents
  print ans
