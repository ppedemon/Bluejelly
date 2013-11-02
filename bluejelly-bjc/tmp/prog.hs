module bluejelly.X(module custom.B,module S) where

import A
import B()
import B hiding()

import qualified custom.B(a,b,c,Eq((==),(/=)))
import qualified container.Set as S hiding(a,C(a,(+)),Z(..),)

x::Int
y::[Char]
a,(+),(-):: forall a b . Prelude.Eq a => a -> a
map :: forall a b . (a -> b) -> A (A x) ([[[a]]],[b])
(>>=) :: forall a b . Monad m => (m (((a)))) -> (a -> b) -> m b
empty :: () => a -> b

polyRank :: (forall a . a -> a) -> 
            (forall a b . (a -> b) -> M a -> b) -> 
            b
            
f :: (C [a] b,D _ _, M [] (->)) => [a] -> [b] -> c
map :: forall a b . (->) a b -> [] a -> [] b
tuple :: (,) Int ((,) a b) -> (Int,(a,b))
g :: (a -> b) -> (c -> d)

type Stack a = [a]

data Eq a => List a = Nil 
                    | forall a . Ord a => Cons a (List a)
                    | Record {a,b,c :: !Int, stack :: forall a. Stack a}
                    | StrictCons !a !(List a)
                    | Existencial (forall a . a -> a) (x.M.T a) (a -> (a->b) -> b) 
newtype T a = T a
data () => Abstract a
data Num a => Complex a = !a :+ !a
data Num a => Cmplx a = (:+) (forall a . a) !a

id x = x
1 = 2
x + y = x + y
(map f) xs = case xs of
  [] -> []
  x:xs -> f x : rest
 where --rest = map f ys ;; ys = tail (x:xs)

(u ||| v) x y z@(Just z) = 
  let 
    Just x = u
    A y = v 
    b = if u == 0 then 1 else 2
  in let in x + y

(Just _, 1 :+ 2, T [1,2,3]) = (34,35)
curry f (x,y) = f x y
[x,y,z] = [1,2,3]

comp = [ (u,v) | 
  x <- [1..], 
  y <- [1,3..], 
  let u = x + y
      v = x - y,
  v >= 0]

monadic f m = do {
  ; x <- m
  ; let z = 1
        y = x + z
  ; z <- f y
  ; return $ x + z ;;;;
}


