
import A
import B
import C

f x = (case x of
  1 -> f (x+1)
  _ -> f (x-1)) :: Int

g x = case x of
  1 -> f (x+1)
  _ -> f (x-1) :: Int

h x = do {
  ; Just x <- m x
  ; return (if x == 1 then 'a' else 'b') } :: Maybe Int

fun x = if x == 1 then 1 else (2+) + f x + case x of {1 -> 2; _ -> -1}

works x = S $ \s -> f s x
monadic m f = m >>= \x -> f x

ops x y = (+) (x `div` y) (x*y)

compose0 f g = f.g
compose1 f g x = f (g x)
compose2 f g = h where h x = f (g x)
compose3 f g = let h = f.g in h
