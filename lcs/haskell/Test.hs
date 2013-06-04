{-# LANGUAGE BangPatterns #-}
module Main where
import Data.Vector.Unboxed.Mutable as MU
import Data.Vector.Unboxed as U hiding (mapM_)
import Criterion.Main
import Criterion.Config
import Diff

config :: Config
config = defaultConfig  { cfgSamples = ljust 100 }

a = U.fromList ['a'..'j'] :: Vector Char
b = U.fromList ['a'..'k'] :: Vector Char

suite :: [Benchmark]
suite = [
          bench "lcs 10" $ whnf (lcs a) b
        ]

main :: IO()
main = defaultMainWith config (return ()) suite
