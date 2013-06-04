{-# LANGUAGE BangPatterns #-}

-- |
-- Implementation of "An O(NP) Sequence Comparison Algorithm" by Sun Wu, Udi
-- Manber, Gene Myers and Web Miller
-- Note: P=(D-(M-N))/2 where D is shortest edit distance, M,N sequence lengths,
-- M >= N
----------------------------------------------------------------------
-- To make the implementation simple while being fast, we use custom
-- dynamic array snakesv to store the snake paths. Once we reach end of both
-- sequences, we just take the last snake path, and follow it back to its pred-
-- -ecessor and so on, until we get to the beginning of the sequence. fp stores
-- the furthest-point. snodes stores location of previous snake path in 
-- snakevec.
----------------------------------------------------------------------
-- 

module Diff 
( lcs )
where
import Data.Vector.Unboxed.Mutable as MU
import Data.Vector.Unboxed as U hiding (mapM_)
import Control.Monad.ST as ST
import Control.Monad.Primitive (PrimState)
import Control.Monad (when) 
import GHC.Float.RealFracMethods (int2Float)
import Data.STRef (newSTRef, modifySTRef, readSTRef)
import Data.Word
--import Criterion.Main
--import Criterion.Config

type MVI1 s  = MVector (PrimState (ST s)) Int
type MVI4 s  = MVector (PrimState (ST s)) (Int,Int,Int,Int)
data Snakev s = S {-# UNPACK #-}!Int 
                                !(MVI4 s) 

cmp :: (U.Unbox a, Eq a) => U.Vector a -> U.Vector a -> Int -> Int -> Int
cmp a b i j = go a b 0 i j
               where
                 go v1 v2 !len !i !j| (i<n) && (j<m) && ((unsafeIndex v1 i) == (unsafeIndex v2 j)) = go v1 v2 (len+1) (i+1) (j+1)
                                    | otherwise = len
                   where
                    n = U.length a
                    m = U.length b

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

sizesnakev :: Snakev s -> Int
sizesnakev (S i _) = i
{-#INLINE sizesnakev #-}

-- Growable array - we always append an element. It grows by factor of 1.5 if more capacity is needed
-- See FB note below about why a growth factor lower than 2 is better
-- https://github.com/facebook/folly/blob/master/folly/docs/FBVector.md
append :: Snakev s -> (Int,Int,Int,Int) -> ST s (Snakev s)
append (S i v) !x = do
   if i < MU.length v then MU.unsafeWrite v i x >> return (S (i+1) v)
   else MU.unsafeGrow v (floor $ 1.5 * (int2Float $ MU.length v)) >>= (\y -> MU.unsafeWrite y i x >> return (S (i+1) y))
   
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

gridWalk :: (U.Unbox a, Eq a) => Vector a -> Vector a -> MVI1 s -> MVI1 s -> Snakev s -> Int -> (Vector a -> Vector a -> Int -> Int -> Int) -> ST s (Snakev s)
gridWalk a b fp snodes snakesv !k cmp = do
   let !offset = 1+U.length a
   (!kp,!yp) <- findYP fp k offset                          
   let xp = yp-k
       len = cmp a b xp yp
       x = xp+len
       y = yp+len
   MU.unsafeWrite fp (k+offset) y  
   snodep <- MU.unsafeRead snodes kp -- get the index of previous snake node in snakev array
   snakesv <- append snakesv (snodep,xp,yp,len)
   MU.unsafeWrite snodes (k+offset) (-1+(sizesnakev snakesv)) -- store the snakev index location of current snake node
   return snakesv
{-#INLINE gridWalk #-}

-- As noted in the paper, we find furthest point given diagonal k, and current set of furthest points. 
-- The function below executes ct times, and updates furthest point (fp), snake node indices in snake
-- array (snodes), and inserts new snakes in snake array as they are found during furthest point search
findSnakes :: (U.Unbox a, Eq a) => Vector a -> Vector a -> MVI1 s -> MVI1 s -> Snakev s -> Int -> Int -> (Vector a -> Vector a -> Int -> Int -> Int) -> (Int -> Int -> Int) -> ST s (Snakev s)
findSnakes a b fp snodes snakesv !k !ct cmp op = do
  U.foldM (\s x -> gridWalk a b fp snodes s (op k x) cmp) snakesv (U.fromList [0..ct-1])
{-#INLINE findSnakes #-}

-- while tail-recursive loop courtesy of Gabriel Gonzalez
-- see http://www.haskellforall.com/2012/01/haskell-for-c-programmers-for-loops.html
while :: (Monad m) => m Bool -> m a -> m ()
while !cond !action = do
      c <- cond
      when c $ do
        action
        while cond action

-- Fills array v at index [i..i+l-1] with values [x..x+l-1]
fill :: MVI1 s -> Int -> Int -> Int -> ST s ()
fill v i x l = U.forM_ (U.fromList [0..l-1]) (\idx -> MU.unsafeWrite v (i+idx) (x+idx))

-- iteration through snake array - we start at last snake in the array which is the snake
-- ending at destination (n,m) when p was found. First element in each entry stores location 
-- of previous snake node in snake array. We set index of next snake to that first element, 
-- and continue iterating backwards until we get to origin (0,0). At origin, the index of
--  previous node is -1. This is used as loop termination condition.
iter :: Snakev s -> MVI1 s -> MVI1 s -> ST s ()
iter (S len v) a b = do
   il <- newSTRef (len-1) -- array index of last snake
   jl <- newSTRef (MU.length a) -- jl keeps track of current location in LCS index arrays a and b
   while (readSTRef il >>= \x -> return (x > (-1))) $ do -- get index of current snake, check for termination
    i <- readSTRef il
    -- read the snake values at index i
    -- p => index of previous node in snake array. (x,y) => starting point of current snake. l => length of snake
    (p,x,y,l) <- MU.unsafeRead v i 
    when (l>0) $ do -- if snake length > 0, match was found - fill up index arrays
      j <- readSTRef jl
      fill a (j-l) x l
      fill b (j-l) y l
      modifySTRef jl (\_ -> j-l) -- l locations filled in LCS indices - adjust for next iteration
    modifySTRef il (\_ -> p) -- set i to previous snake node

-- Helper function for lcs     
lcsh :: (U.Unbox a, Eq a) => Vector a -> Vector a -> Bool -> (Vector Int,Vector Int)
lcsh a b flip = runST $ do
  let n = U.length a
      m = U.length b
      delta = m-n
      offset=n+1
      deloff=m+1 -- array index location of diagonal at delta
  fp <- newVI1 (m+n+3) (-1) -- array of furthest points
  snodes <- newVI1 (m+n+3) (-1) -- array to keep track of snake nodes in each iteration
  snakesv <- newSnakes (m+n+1) -- array of snakes - how does that sound?
  p <- newSTRef 0 -- minimum number of deletions for a
  s <- newSTRef snakesv -- store reference to snakevector - we have to pass it around
  while (MU.unsafeRead fp (deloff) >>= \x -> return $! (x<m))
    $ do 
      n <- readSTRef p
      s0 <- readSTRef s
      s1 <- findSnakes a b fp snodes s0 (-n) (delta+n) cmp (+) -- vertical traversal
      s2 <- findSnakes a b fp snodes s1 (delta+n) n cmp (-) -- horizontal traversal
      s3 <- findSnakes a b fp snodes s2 delta 1 cmp (-) -- diagonal traversal
      modifySTRef s (\_ -> s3) -- store the latest snakevec for next iteration
      modifySTRef p (+1) -- increment p by 1
  -- length of LCS is n-p. p must be decremented by 1 first for correct value of p 
  lcslen <- (readSTRef p >>= \x -> return $ (U.length a)-(x-1)) 
  snakesv <- readSTRef s
  -- arrays to store LCS indices for a and b respectively
  ai <- MU.new lcslen 
  bi <- MU.new lcslen
  -- iterate through snake vector, and fill up ai,bi with indices of match
  iter snakesv ai bi 
  ai <- U.unsafeFreeze ai
  bi <- U.unsafeFreeze bi
  -- maintain correct order when returning indices
  if flip then return (bi,ai) 
  else return (ai,bi)

-- Function to find longest common subsequence given unboxed vectors a and b
-- It returns indices of LCS in a and b
lcs :: (U.Unbox a, Eq a) => Vector a -> Vector a -> (Vector Int,Vector Int)
lcs a b | (U.length a > U.length b) = lcsh b a True
        | otherwise = lcsh a b False
{--
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
--}
