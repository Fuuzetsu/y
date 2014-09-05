{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Criterion.Main as C
import Control.DeepSeq
import Data.List (foldl')
import qualified Y.String as S
import qualified Data.Rope as R

instance NFData R.Rope where
  rnf r = r `seq` ()

benchCons :: String -> String -> C.Benchmark
benchCons text name
    = C.bench name $ C.nf go ""
    where go start = foldr S.cons start text


benchConsRope :: String -> String -> C.Benchmark
benchConsRope text name
    = C.bench name $ C.nf go (R.fromString "")
    where go start = foldr R.cons start text


benchSnoc :: String -> String -> C.Benchmark
benchSnoc text name
    = C.bench name $ C.nf go ""
    where go start = foldl' S.snoc start text

benchSnocRope :: String -> String -> C.Benchmark
benchSnocRope text name
    = C.bench name $ C.nf go (R.fromString "")
    where go start = foldl' R.snoc start text

benchLength :: S.YiString -> String -> C.Benchmark
benchLength text name
    = C.bench name
    $ C.nf S.length text

benchLines :: S.YiString -> String -> C.Benchmark
benchLines text name
    = C.bench name
    $ C.nf (S.splitOnNewLines :: S.YiString -> [S.YiString]) text

benchDrop :: S.YiString -> String -> C.Benchmark
benchDrop text name
    = C.bench name
    $ C.nf (\x -> foldr S.drop x (replicate 1000 (S.Size 1))) text


benchDropRope :: R.Rope -> String -> C.Benchmark
benchDropRope text name
    = C.bench name
    $ C.nf (\x -> foldr R.drop x (replicate 1000 1)) text

benchTake :: S.YiString -> String -> C.Benchmark
benchTake text name
    = C.bench name
    $ C.nf (\x -> foldr S.take x [1000, 999 .. 1]) text


benchTakeRope :: R.Rope -> String -> C.Benchmark
benchTakeRope text name
    = C.bench name
    $ C.nf (\x -> foldr R.take x [1000, 999 .. 1]) text

benchSplitAt :: S.YiString -> String -> C.Benchmark
benchSplitAt text name
    = C.bench name
    $ C.nf (\x -> foldr ((fst .) . S.splitAt) x [1000, 999 .. 1]) text

benchSplitAtRope :: R.Rope -> String -> C.Benchmark
benchSplitAtRope text name
    = C.bench name
    $ C.nf (\x -> foldr ((fst .) . R.splitAt) x [1000, 999 .. 1]) text

main :: IO ()
main = C.defaultMain
    [ benchCons longText "cons long"
    , benchConsRope longText "cons long rope"
    , benchCons wideText "cons wide"
    , benchConsRope wideText "cons wide rope"
    , benchSnoc longText "snoc long"
    , benchSnocRope longText "snoc long rope"
    , benchSnoc wideText "snoc wide"
    , benchSnocRope wideText "snoc wide rope"
    , benchLines longYiString "lines long"
    , benchLines wideYiString "lines wide"
    , benchDrop longYiString "drop long"
    , benchDropRope longYiStringRope "drop long rope"
    , benchDrop wideYiString "drop wide"
    , benchDropRope wideYiStringRope "drop wide rope"
    , benchTake longYiString "take long"
    , benchTakeRope longYiStringRope "take long rope"
    , benchTake wideYiString "take wide"
    , benchTakeRope wideYiStringRope "take wide rope"
    , benchSplitAt longYiString "splitAt long"
    , benchSplitAtRope longYiStringRope "splitAt long rope"
    , benchSplitAt wideYiString "splitAt wide"
    , benchSplitAtRope wideYiStringRope "splitAt wide rope"
    ]

longText :: String
longText = force . unlines
         $ replicate 1000 "Lorem ipsum dolor sit amet"
{-# NOINLINE longText #-}

longYiString :: S.YiString
longYiString = force (S.fromString longText)
{-# NOINLINE longYiString #-}

longYiStringRope :: R.Rope
longYiStringRope = force (R.fromString longText)
{-# NOINLINE longYiStringRope #-}

wideText :: String
wideText = force . unlines
         $ replicate 10 . concat
         $ replicate 100 "Lorem ipsum dolor sit amet "
{-# NOINLINE wideText #-}

wideYiString :: S.YiString
wideYiString = force (S.fromString wideText)
{-# NOINLINE wideYiString #-}

wideYiStringRope :: R.Rope
wideYiStringRope = force (R.fromString wideText)
{-# NOINLINE wideYiStringRope #-}
