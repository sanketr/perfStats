{-# LANGUAGE BangPatterns #-}

-- |
-- Implementation of "An O(NP) Sequence Comparison Algorithm" by Sun Wu, Udi
-- Manber, Gene Myers and Web Miller
-- Note: P=(D-(M-N))/2 where D is shortest edit distance, M,N sequence lengths,
-- M >= N
--

import Data.Vector.Unboxed.Mutable as MU
import Data.Vector.Unboxed as U hiding (mapM_)
import Control.Monad.ST as ST
import Control.Monad.Primitive (PrimState)
import Control.Monad as CM (when,forM_)
import Data.Int

type MVI1 s  = MVector (PrimState (ST s)) Int

cmp :: U.Vector Int32 -> U.Vector Int32 -> Int -> Int -> Int
cmp a b i j = go 0 i j
               where
                 n = U.length a
                 m = U.length b
                 go len i j| (i<n) && (j<m) && ((unsafeIndex a i) == (unsafeIndex b j)) = go (len+1) (i+1) (j+1)
                              | otherwise = len
{-#INLINE cmp #-}

-- function to find previous y on diagonal k for furthest point
findYP :: MVI1 s -> Int -> Int -> ST s Int
findYP fp k offset = do
              y0 <- MU.unsafeRead fp (k+offset-1) >>= \x -> return $ 1+x
              y1 <- MU.unsafeRead fp (k+offset+1)
              if y0 > y1 then return y0
              else return y1
{-#INLINE findYP #-}

gridWalk :: Vector Int32 -> Vector Int32 -> MVI1 s -> Int -> Int -> ST s ()
gridWalk a b fp k offset = do
   yp <- findYP fp k offset
   MU.unsafeWrite fp (k+offset) (yp + (cmp a b (yp-k) yp))
{-#INLINE gridWalk #-}

-- As noted in the paper, we find furthest point given diagonal k, and current set of furthest points.
-- The function below executes ct times, and updates furthest point (fp), snake node indices in snake
-- array (snodes), and inserts new snakes in snake array as they are found during furthest point search
findSnakes :: Vector Int32 -> Vector Int32 -> MVI1 s ->  Int -> Int -> Int -> (Int -> Int -> Int) -> ST s ()
findSnakes a b fp k ct offset op = go 0
     where go x 
            | x < ct = gridWalk a b fp (op k x) offset >> go (x+1)
            | otherwise = return ()
{-#INLINE findSnakes #-}

forMi :: ST s Bool -> (Int -> ST s ()) -> ST s Int
forMi p f = go 0 
    where go n = do
              x <- p
              if x
                 then f n >> go (n+1)
                 else return n

-- Helper function for lcs
lcsh :: Vector Int32 -> Vector Int32 -> Int
lcsh a b = runST $ do
  let n = U.length a
      m = U.length b
      delta = m-n
      offset=n+1
      deloff=m+1 -- index location of diagonal at delta in "furthest point" array
  fp <- MU.replicate (m+n+3) (-1) -- array of furthest points
  p <- forMi (MU.unsafeRead fp deloff >>= \x -> return $ (x<m)) 
     (\i -> findSnakes a b fp (-i) (delta+i) offset (+) >>
       findSnakes a b fp (delta+i) i offset (-) >>
       findSnakes a b fp delta 1 offset (-))  
  return $ (U.length a)-(p-1)
{-#INLINABLE lcsh #-}

-- Function to find longest common subsequence given unboxed vectors a and b
-- It returns indices of LCS in a and b
lcs :: Vector Int32 -> Vector Int32 -> Int
lcs a b | (U.length a > U.length b) = lcsh b a
        | otherwise = lcsh a b
{-#INLINABLE lcs #-}

main = print $ lcs (U.fromList [0..3999]) (U.fromList [3999..7999])
