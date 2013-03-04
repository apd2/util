{-#LANGUAGE DeriveDataTypeable, TupleSections #-}
--Generic utility functions
module Util(
    log2,
    b2IntMSF, 
    b2IntLSF,
    index,
    allComb,
    stateMap,
    if',
    alt,
    pad,
    bitsToBoolArrLe,
    bitsToBoolArrBe,
    boolArrToBitsLe,
    boolArrToBitsBe,
    debug,
    debugm,
    prints,
    count,
    filterBy,
    indexMaybe,
    slice,
    debugp,
    debugf,
    showRes,
    indexes,
    indexesDef,
    trace,
    deepTrace,
    deepTrace2,
    (.*),
    (.**),
    unfoldr2,
    merge,
    mergeLists,
    fromJustMsg,
    iterateN,
    chainM,
    mapAccumLM,
    chain,
    Named(..),
    withName,
    aggregate,
    mapIdx,
    mapIdxM,
    foldIdx,
    foldIdxM,
    mapFst,
    mapSnd,
    map2,
    map3,
    mapFst3,
    mapSnd3,
    mapTrd3,
    mapFst4,
    mapSnd4,
    mapTrd4,
    mapFrt4,
    getIORef,
    pairs,
    sortAndGroup) where

import Data.Bits
import System.IO.Unsafe
import Control.DeepSeq
import Control.Monad
import Data.Data
import Data.List
import Data.Typeable
import Data.Map (Map)
import Data.IORef
import qualified Data.Map as Map

--Logarithm to base 2. Equivalent to floor(log2(x))
log2 :: Integer -> Int
log2 0 = 0
log2 1 = 0
log2 n 
    | n>1 = 1 + log2 (n `div` 2)
    | otherwise = error "log2: negative argument"

--Calculate the int representstion of a list of bools where the most significant bit appears first
b2IntMSF :: [Bool] -> Integer
b2IntMSF = foldl (\x y -> (x*2) + (toInteger $ fromEnum y)) 0 

--Calculate the int representation of a list of bools where the least significant bit appears first
b2IntLSF :: [Bool] -> Int
b2IntLSF = b2Int' 1
b2Int' c [] = 0
b2Int' c (x:xs) = (fromEnum x * c) + b2Int' (c*2) xs

--Version of (!!) that takes a default argument for the case when the index is greater than the list length
index :: [a] -> a-> Int -> a
index xs d n | n < 0  = error "index: negative index"
index [] d _          = d
index (x:_) d 0       = x
index (_:xs) d n      = index xs d (n-1)

--Returns all combinations of a list of lists. Best illustrated with an example:
--allcomb [[1, 2], [3], [2, 3, 4]] = [[1, 3, 2], [1, 3, 3], [1, 3, 4], [2, 3, 2], [2, 3, 3], [2, 3, 4]]
allComb :: [[a]] -> [[a]]
allComb [] = [[]]
--allComb ([]:xs) = allComb xs
allComb (x:xs) = concatMap (h (allComb xs)) x
	where h xs x = map (x :) xs

stateMap :: [a -> (b, a)] -> a -> (a, [b])
stateMap [] s = (s, [])
stateMap (f:fs) s = 
    let (a, s') = f s
        (s'', res) = stateMap fs s'
    in  (s'', a : res)

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

alt :: a -> a -> Bool -> a
alt x _ True  = x
alt _ y False = y

pad :: Int -> a -> [a] -> [a]
pad i d a = a ++ replicate (i - length a) d

bitsToBoolArrLe :: (Bits b) => b -> [Bool]
bitsToBoolArrLe e = map (testBit e) [0..]

bitsToBoolArrBe :: (Bits b) => Int -> b -> [Bool]
bitsToBoolArrBe s e = reverse $ take s $ map (testBit e) [0..]

boolArrToBitsLe :: (Bits a, Num a) => [Bool] -> a
boolArrToBitsLe bits = foldIdx (\x bit id -> if bit then setBit x id else x) 0 bits

boolArrToBitsBe :: (Bits a, Num a) => [Bool] -> a
boolArrToBitsBe bits = foldIdx (\x bit id -> if bit then setBit x id else x) 0 (reverse bits)

{-# DEPRECATED debugp, debugf, prints, debugm, showRes "Use Debug.TraceUtils instead" #-}
--debug print in monad
debug s = unsafePerformIO (print s) `seq` return ()
--debug print before evaluating to whnf
prints s f = unsafePerformIO (print s) `seq` f
--debug print result of computation with message
debugm msg s f = unsafePerformIO (putStrLn (msg ++ show s)) `seq` f
--debug print result of computation
debugp s = unsafePerformIO (print s) `seq` s
--debug print function of result of computation
debugf f s = unsafePerformIO (putStrLn (f s)) `seq` s
showRes m f c = unsafePerformIO (putStrLn (m ++ f c)) `seq` c
--debug print before and after whnf evaluation
trace s f = unsafePerformIO $ do
    print s
    f `seq` print (s ++ " done")
    return f
--debug print before and after complete evaluation
deepTrace s f = unsafePerformIO $ do
    print s
    f `deepseq` print (s ++ " done")
    return f
--pretty version of above
deepTrace2 s f = unsafePerformIO $ do
    putStr s
    f `deepseq` putStrLn ("done")
    return f

count f = length . filter f

filterBy :: (a->Bool) -> [a] -> [b] -> [b]
filterBy _ [] [] = []
filterBy f (a:as) (b:bs) 
    | f a = b : filterBy f as bs
    | otherwise = filterBy f as bs
filterBy _ _ _ = [] --error "filterBy: lists not same length"

indexMaybe :: [a] -> Int -> Maybe a
indexMaybe xs n | n < 0 = error "indexMaybe: negative index"
indexMaybe [] _         = Nothing
indexMaybe (x:_) 0      = Just x
indexMaybe (_:xs) n     = indexMaybe xs (n-1)

slice :: Int -> Int -> [a] -> Maybe [a]
slice _ _ [] = Just []
slice k n xs | k > n || k >= length xs || n >= length xs || k < 0 || n < 0 = Nothing
             | otherwise = Just (drop k $ take (n+1) xs)

--Assumes "is" is in ascending order
indexes :: [Int] -> [a] -> [a]
indexes is values = indexes' 0 is values
    where 
    indexes' offset ia@(i:is) (l:ls)
        | i == offset = l : indexes' (offset+1) is ls
        | otherwise   = indexes' (offset+1) ia ls
    indexes' _ _ _ = []

indexesDef :: a -> [Int] -> [a] -> [a]
indexesDef def is values = indexes' 0 is values
    where 
    indexes' offset ia@(i:is) (l:ls)
        | i == offset = l : indexes' (offset+1) is ls
        | otherwise   = indexes' (offset+1) ia ls
    indexes' _ is _ = replicate (length is) def

(.*) = (.) . (.)
infix 8 .*

(.**) = (.) . (.) . (.)
infix 8 .**

--same as unfoldr but returns the final state
unfoldr2 :: (b -> Maybe (a, b)) -> b -> ([a], b)
unfoldr2 f b =
    case f b of
        Just (a,new_b) -> (a : rest, finst)
            where
            (rest, finst) = unfoldr2 f new_b
        Nothing        -> ([], b)

merge pred xs [] = xs
merge pred [] ys = ys
merge pred (x:xs) (y:ys) =
  case pred x y of
      True  -> x: merge pred xs (y:ys)
      False -> y: merge pred (x:xs) ys

mergeLists pred [] = []
mergeLists pred (x:[]) = x
mergeLists pred (x:xs) = merge pred x (mergeLists pred xs)

fromJustMsg :: String -> Maybe a -> a
fromJustMsg _ (Just a) = a
fromJustMsg m Nothing  = error m

iterateN :: (a -> a) -> a -> Int -> a
iterateN f x 0 = x
iterateN f x n = iterateN f (f x) (n-1)

chainM :: Monad m => [a -> m a] -> a -> m a
chainM funcs init = foldl (>>=) (return init) funcs 

mapAccumLM :: Monad m => (acc -> x -> m (acc, y)) -> acc -> [x] -> m (acc, [y])
mapAccumLM _ s []     = return (s, [])
mapAccumLM f s (x:xs) = do
    (s1, x')  <- f s x
    (s2, xs') <- mapAccumLM f s1 xs
    return    (s2, x' : xs')

chain :: [a -> a] -> a -> a
chain = flip $ foldl $ flip ($)

data Named a = Named {name :: String, nValue :: a} deriving (Typeable,Data,Eq,Ord)

instance (NFData a) => NFData (Named a) where
	rnf (Named x y) = rnf x `seq` rnf y

instance Show a => Show (Named a) where
    show (Named name value) = name ++ ": " ++ show value

instance Functor Named where
    fmap f v = Named (name v) (f $ nValue v)

withName :: (String -> a -> b) -> Named a -> b
withName func (Named n v) = func n v

aggregate :: (Ord a) => [(a, b)] -> [(a, [b])]
aggregate args = Map.toList $ foldl f Map.empty args
    where
    f mp (a, b) = case Map.lookup a mp of
        Just x -> Map.insert a (b:x) mp
        Nothing -> Map.insert a [b] mp

mapIdx :: (a -> Int -> b) -> [a] -> [b]
mapIdx f xs = map (uncurry f) $ zip xs [0..]

mapIdxM :: (Monad m) => (a -> Int -> m b) -> [a] -> m [b]
mapIdxM f xs = mapM (uncurry f) $ zip xs [0..]

foldIdx :: (a -> b -> Int -> a) -> a -> [b] -> a
foldIdx f acc xs = foldl' (\acc (x,idx) -> f acc x idx) acc $ zip xs [0..]

foldIdxM :: (Monad m) => (a -> b -> Int -> m a) -> a -> [b] -> m a
foldIdxM f acc xs = foldM (\acc (x,idx) -> f acc x idx) acc $ zip xs [0..]

-- Tuples
mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x,y) = (f x,y)
mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (x,y) = (x,f y)

map2 :: (a->b, c->d) -> (a,c) -> (b,d)
map2 (f1,f2) (x,y) = (f1 x, f2 y)

map3 :: (a->b, c->d, e->f) -> (a,c,e) -> (b,d,f)
map3 (f1,f2,f3) (x,y,z) = (f1 x, f2 y, f3 z)

mapFst3 f (x1,x2,x3) = (f x1,x2,x3)
mapSnd3 f (x1,x2,x3) = (x1,f x2,x3)
mapTrd3 f (x1,x2,x3) = (x1,x2,f x3)

mapFst4 f (x1,x2,x3,x4) = (f x1,x2,x3,x4)
mapSnd4 f (x1,x2,x3,x4) = (x1,f x2,x3,x4)
mapTrd4 f (x1,x2,x3,x4) = (x1,x2,f x3,x4)
mapFrt4 f (x1,x2,x3,x4) = (x1,x2,x3,f x4)

-- IORef

getIORef :: (a -> b) -> IORef a -> IO b
getIORef f = liftM f . readIORef

-- unique pairs of array elements,
-- excluding reflective pairs (x,x) and symmetric pairs (x,y), (y,x)
pairs :: [a] -> [(a,a)]
pairs [] = []
pairs [x] = []
pairs (x:xs) = (map (x,) xs) ++ pairs xs

-- Group elements in the list (unlike groupBy, this function groups all
-- equivalent elements, not just adjacent ones)
sortAndGroup :: (Ord b) => (a -> b) -> [a] -> [[a]]
sortAndGroup f = groupBy (\x y -> f x == f y) .
                 sortBy (\x y -> compare (f x) (f y))


