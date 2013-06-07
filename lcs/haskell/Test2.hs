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
import Data.STRef (newSTRef, modifySTRef, readSTRef, writeSTRef)
import Data.Int

type MVI1 s  = MVector (PrimState (ST s)) Int

cmp :: U.Vector Int32 -> U.Vector Int32 -> Int -> Int -> Int
cmp a b i j = go 0 i j
               where
                 n = U.length a
                 m = U.length b
                 go !len !i !j| (i<n) && (j<m) && ((unsafeIndex a i) == (unsafeIndex b j)) = go (len+1) (i+1) (j+1)
                                    | otherwise = len
{-#INLINE cmp #-}

newVI1 :: Int -> Int -> ST s (MVI1 s)
newVI1 n !x = do
          a <- new n
          mapM_ (\i -> MU.unsafeWrite a i x) [0..n-1]
          return a

-- function to find previous y on diagonal k for furthest point
findYP :: MVI1 s -> Int -> Int -> ST s Int
findYP fp k offset = do
              y0 <- MU.unsafeRead fp (k+offset-1) >>= \x -> return $ 1+x
              y1 <- MU.unsafeRead fp (k+offset+1)
              if y0 > y1 then return y0
              else return y1
{-#INLINE findYP #-}

gridWalk :: Vector Int32 -> Vector Int32 -> MVI1 s -> Int -> ST s ()
gridWalk a b fp !k = do
   let !offset = 1+U.length a
   !yp <- {-#SCC findYP #-} findYP fp k offset
   {-#SCC cmp #-} MU.unsafeWrite fp (k+offset) (yp + (cmp a b (yp-k) yp))
{-#INLINE gridWalk #-}

-- As noted in the paper, we find furthest point given diagonal k, and current set of furthest points.
-- The function below executes ct times, and updates furthest point (fp), snake node indices in snake
-- array (snodes), and inserts new snakes in snake array as they are found during furthest point search
findSnakes :: Vector Int32 -> Vector Int32 -> MVI1 s ->  Int -> Int -> (Int -> Int -> Int) -> ST s ()
findSnakes a b fp !k !ct !op = {-#SCC findSnakes #-} go  0
     where go x
            | x < ct = gridWalk a b fp (op k x) >> go (x+1)
            | otherwise = return ()
{-#INLINE findSnakes #-}

whileM_ :: ST s Bool -> ST s a -> ST s ()
whileM_ p f = go
    where go = do
            x <- p
            if x
                then f >> go
                else return ()

-- Helper function for lcs
lcsh :: Vector Int32 -> Vector Int32 -> Int
lcsh a b = runST $ do
  let n = U.length a
      m = U.length b
      delta = m-n
      offset=n+1
      deloff=m+1 -- index location of diagonal at delta in "furthest point" array
  fp <- newVI1 (m+n+3) (-1) -- array of furthest points
  p <- newSTRef 0 -- minimum number of deletions for a
  whileM_ (MU.unsafeRead fp (deloff) >>= \x -> return $! (x<m))
    $ do
      n <- readSTRef p
      findSnakes a b fp (-n) (delta+n) (+) -- vertical traversal
      findSnakes a b fp (delta+n) n (-) -- horizontal traversal
      findSnakes a b fp delta 1 (-) -- diagonal traversal
      modifySTRef p (+1) -- increment p by 1
  -- length of LCS is n-p. p must be decremented by 1 first for correct value of p
  readSTRef p >>= \x -> return $ (U.length a)-(x-1)
{-#INLINABLE lcsh #-}

-- Function to find longest common subsequence given unboxed vectors a and b
-- It returns indices of LCS in a and b
lcs :: Vector Int32 -> Vector Int32 -> Int
lcs a b | (U.length a > U.length b) = lcsh b a
        | otherwise = lcsh a b
{-#INLINABLE lcs #-}

main = print $ lcs (U.fromList [0..3999]) (U.fromList [3999..7999])
