package bluejelly.bjc.monad

import scalaz._
import syntax.monad._

trait MonadSupply[F[_]] extends Monad[F] {
  def fresh:F[Int]
}

object MonadSupply extends MonadSupplyInstances {
  private[monad] type StateTF[F[_],S] = {
    type t[x] = StateT[F,S,x]
  }

  def apply[F[_]](implicit F:MonadSupply[F]) = F
  def freshName[F[_]](implicit F:MonadSupply[F]):F[String] = (("$" + (_:Int)):LiftV[F,Int,String]).lift.apply(F.fresh)
}

import MonadSupply.StateTF

sealed abstract class MonadSupplyInstances {
  implicit def stateTMonadSupply[F[_],S](implicit F0:MonadSupply[F]):MonadSupply[StateTF[F,S]#t] = 
    new StateTMonadSupply[F,S] {
      implicit def F:MonadSupply[F] = F0 
    }
}

private trait StateTMonadSupply[F[_],S] extends MonadSupply[StateTF[F,S]#t] {
  implicit def F:MonadSupply[F]

  // Duplication!
  def point[A](a: =>A):StateT[F,S,A] = StateT.stateT(a)
  def bind[A,B](fa:StateT[F,S,A])(f:A => StateT[F,S,B]):StateT[F,S,B] = fa >>= f

  def fresh:StateT[F,S,Int] =
    F.fresh.liftM[({type Lambda[F[_],A] = StateT[F,S,A]})#Lambda]
}

/*
 * TODO: Remove this!
 */
object test {
  import Id._
  import SupplyT._
  import MonadSupply._
  
  val m:Supply[String] = supplyT("a")
  val x = m >>= (a => supplyT("a"+1))
  val names = for {
    n0 <- freshName[Supply]
    n1 <- freshName[Supply]
  } yield List(n0,n1)
  val ns = names.eval(0)

  type StateSupply[A] = StateT[Supply,String,A]
  val z = for {
    n0 <- freshName[StateSupply]
    n1 <- freshName[StateSupply]
  } yield List(n0,n1)
  val zs = z.eval("empty").eval(0)
}
