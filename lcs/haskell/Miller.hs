{-# LANGUAGE BangPatterns #-}
module Main where
import Data.Vector.Unboxed.Mutable as MU
import Data.Vector.Unboxed as U hiding (mapM_,forM_)
import Control.Monad.ST as ST
import Control.Monad.Primitive (PrimState)
import Control.Monad (when) 
import GHC.Float.RealFracMethods (int2Float)
import Data.STRef (newSTRef, modifySTRef, readSTRef)

cmp :: (U.Unbox a, Eq a) => U.Vector a -> U.Vector a -> Int -> Int -> Int
cmp a b i j = go a b 0 i j
               where
                 go v1 v2 !len !i !j| (i<n) && (j<m) && ((unsafeIndex v1 i) == (unsafeIndex v2 j)) = go v1 v2 (len+1) (i+1) (j+1)
                                    | otherwise = len
                   where
                    n = U.length a
                    m = U.length b

type MVI1 s  = MVector (PrimState (ST s)) Int
type MVI4 s  = MVector (PrimState (ST s)) (Int,Int,Int,Int)
data Snakev s = S {-# UNPACK #-}!Int 
                                (MVI4 s) 

newVI1 :: Int -> Int -> ST s (MVI1 s)
newVI1 n x = do 
          a <- new n
          mapM_ (\i -> MU.unsafeWrite a i x) [0..n-1]
          return a 

-- Return dynamic array of snakes with current index set at 0
newSnakes :: Int -> ST s (Snakev s)
newSnakes n = do
            v <- new n
            return (S 0 v)

test2 :: Int -> ST s (Vector Int)
test2 n = newVI1 n (-1) >>= U.unsafeFreeze

sizev :: Snakev s -> Int
sizev (S i _) = i

append :: Snakev s -> (Int,Int,Int,Int) -> ST s (Snakev s)
append (S i v) x = do
   if i < MU.length v then MU.unsafeWrite v i x >> return (S (i+1) v)
   else MU.unsafeGrow v (floor $ 1.5 * (int2Float $ MU.length v)) >>= (\y -> MU.unsafeWrite y i x >> return (S (i+1) y))
{-#INLINE append #-}
   
-- function to find previous y on diagonal k for furthest point 
findYP :: MVI1 s -> Int -> Int -> ST s (Int,Int)
findYP fp k offset = do
              let k0 = k+offset-1
                  k1 = k+offset+1
              y0 <- MU.unsafeRead fp k0 >>= \x -> return $ 1+x
              y1 <- MU.unsafeRead fp k1
              if y0 > y1 then return (k0,y0)
              else return (k1,y1)
{-#INLINE findYP #-}

gridWalk :: Vector Int -> Vector Int -> MVI1 s -> MVI1 s -> Snakev s -> Int -> (Vector Int -> Vector Int -> Int -> Int -> Int) -> ST s (Snakev s)
gridWalk a b fp snodes snakesv k cmp = do
   let offset = 1+U.length a
   (!kp,!yp) <- findYP fp k offset                          
   let xp = yp-k
       len = cmp a b xp yp
       x = xp+len
       y = yp+len
   MU.unsafeWrite fp (k+offset) y  
   snodep <- MU.unsafeRead snodes kp -- get the previous snake node
   snakesv <- append snakesv (snodep,xp,yp,len)
   MU.unsafeWrite snodes (k+offset) (-1+(sizev snakesv))
   return snakesv
{-#INLINE gridWalk #-}

findSnakes :: Vector Int -> Vector Int -> MVI1 s -> MVI1 s -> Snakev s -> Int -> Int -> (Vector Int -> Vector Int -> Int -> Int -> Int) -> (Int -> Int -> Int) -> ST s (Snakev s)
findSnakes a b fp snodes snakesv k ct cmp op = do
  U.foldM (\s x -> gridWalk a b fp snodes s (op k x) cmp) snakesv (U.fromList [0..ct-1])

while :: (Monad m) => m Bool -> m a -> m ()
while cond action = do
      c <- cond
      when c $ do
        action
        while cond action

lcsh :: Vector Int -> Vector Int -> Bool -> Int
lcsh a b flip = runST $ do
  let n = U.length a
      m = U.length b
      delta = m-n
      offset=n+1
  fp <- newVI1 (m+n+3) (-1)
  snodes <- newVI1 (m+n+3) (-1)
  snakesv <- newSnakes (m+n+1)
  p <- newSTRef 0
  s <- newSTRef snakesv
  while (MU.unsafeRead fp (delta+offset) >>= \x -> return (x<m))
    $ do 
      n <- readSTRef p
      s0 <- readSTRef s
      s1 <- findSnakes a b fp snodes s0 (-n) (delta+n) cmp (+)
      s2 <- findSnakes a b fp snodes s1 (delta+n) n cmp (-)
      s3 <- findSnakes a b fp snodes s2 delta 1 cmp (-)
      modifySTRef s (\_ -> s3)
      modifySTRef p (+1)
  readSTRef p >>= \x -> return $ x-1

lcs :: Vector Int -> Vector Int -> Int
lcs a b | (U.length a > U.length b) = lcsh b a True
        | otherwise = lcsh a b False

main = print $ lcsh (U.fromList [1..3])  (U.fromList [1..5]) False
