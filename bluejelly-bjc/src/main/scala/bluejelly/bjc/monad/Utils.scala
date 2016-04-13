package bluejelly.bjc.monad

import scalaz._
import syntax.monad._

object Utils {
  // Scalaz doesn't define a traversable instance for the (?,B) type
  def traverseFst[F[_]:Applicative,A,B,C](f:A => F[C])(t:(A,B)):F[(C,B)] = t match {
    case (a,b) => f(a) map ((_,b))
  }

  // Auxiliary function to create a monadic value for a (state,value) pair, for *any* monad
  def defS[M[_]:Monad,S,A](s:S,a:A) = implicitly[Monad[M]].point((s,a)) 
}
