{-# LANGUAGE BangPatterns #-}
module Main where
import Data.Vector.Unboxed.Mutable as MU
import Data.Vector.Unboxed as U hiding (mapM_)
import Criterion.Main
import Criterion.Config
import Diff
import Data.Word

config :: Config
config = defaultConfig  { cfgSamples = ljust 100 }

a = U.fromList [0..9] :: Vector Word8
b = U.fromList [0..10] :: Vector Word8

suite :: [Benchmark]
suite = [
          bench "lcs 10" $ whnf (lcs a) b
        ]
{-#SPECIALIZE lcs :: Vector Word8 -> Vector Word8 -> (Vector Int, Vector Int) #-}

main :: IO()
main = defaultMainWith config (return ()) suite
