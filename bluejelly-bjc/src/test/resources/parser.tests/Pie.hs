module Main where
 
import control.Monad
import container.array.IArray
import qualified container.Map as M
 
data Ingredient = E|D|F|C|M deriving (Eq,Ord,Show)
 
type Pie = Array (Int,Int) Ingredient
 
liftIf :: MonadPlus m => (a -> Bool) -> (a -> a) -> a -> m a
liftIf p f a = if p a then return (f a) else mzero
 
row i p = [p!(i,c) | c <- [x..y]]
  where ((_,x),(_,y)) = bounds p
 
col i p = [p!(r,i) | r <- [x..y]]
  where ((x,_),(y,_)) = bounds p
 
emptyPie lo hi = listArray (lo,hi) (repeat E)
 
growPie ix p m = [(p//[(ix,x)], M.update (liftIf (>1) (subtract 1)) x m) | x <- M.keys m]
 
allPies :: (Int,Int) -> (Int,Int) -> [(Ingredient,Int)] -> [Pie]
allPies lo hi ilist = map fst $
  foldl (\ps i -> concatMap (uncurry $ growPie i) ps) initial is
    where initial = [(pie, M.fromList ilist)]
          pie = emptyPie lo hi
          is = indices pie
 
--tasty p = (any tastyList [row i p | i <- [u..v]]) || (any tastyList [col i p | i <- [x..y]])
tasty p = (all tastyList [row i p | i <- [u..v]]) || (all tastyList [col i p | i <- [x..y]])
  where tastyList (M:xs) = allSame xs
        tastyList (x:xs) = all (\y -> y == x || y == M) xs
        allSame [] = True
        allSame (x:xs) = all (==x) xs
        ((u,x),(v,y)) = bounds p
 
-- 8112 tasty pies?
numTasty = length (filter tasty ps)
  where ps = allPies (1,1) (3,3) [(D,3),(F,3),(C,3),(M,1)]

------------------------------------------------------------------------------------------

{-
perms [] = [[]]
perms (x:xs) = concatMap (insert x) (perms xs)
  where insert x [] = [[x]]
        insert x zs@(y:ys) = (x:zs) : map (y:) (insert x ys) 

transpose a = ixmap (bounds a) (\(i,j)->(j,i)) a

harrs :: [Pie]
harrs = [listArray ((1,1),(3,3)) (xs ++ xs ++ xs) | xs <- perms [D,F,C]]

varrs :: [Pie]
varrs = map transpose harrs

m a = [a//[(ix,M)] | ix <- indices a]

sol = concatMap m harrs ++ concatMap m varrs

hasM a = any (==M) (elems a)
-}

