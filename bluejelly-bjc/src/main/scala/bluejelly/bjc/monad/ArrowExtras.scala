package bluejelly.bjc.monad

import scalaz._
import scalaz.Kleisli._
import scalaz.LazyTuple._
import std.option._
import syntax.monad._

trait ArrowLoop[=>:[_,_]] {
  def loop[B,C,D](f: (B :&: D) =>: (C :&: D))(implicit A:Arrow[=>:]):B =>: C
}

trait ArrowPlus[=>:[_,_]] {
  def aempty[B,C](implicit A:Arrow[=>:]):B =>: C
  def aplus[B,C](f:B =>: C, g:B =>: C)(implicit A:Arrow[=>:]):B =>: C
}

import KleisliArrow._

// -----------------------------------------------------------------------
// Define ArrowLoop and ArrowPlus instances for Kleisli[M,_,_]
// -----------------------------------------------------------------------
sealed trait KleisliArrowLoopInstance[M[_]] extends ArrowLoop[K[M]#t] {
  type =>:[B,C] = K[M]#t[B,C]

  implicit def F:Monad[M]
  implicit def MF:MonadFix[M]
  implicit def A:Arrow[=>:]

  def loop[B,C,D](f:(B :&: D) =>: (C :&: D))(implicit A:Arrow[=>:]):B =>: C = {
    val f1: B => (=> (C :&: D)) => M[C :&: D] = x => y => f(lazyTuple2(x, y._2))
    kleisli(b => MF.mfix(f1(b)).map(_._1))
  }
}

sealed trait KleisliArrowPlusInstance[M[_]] extends ArrowPlus[K[M]#t] {
  type =>:[B,C] = K[M]#t[B,C]

  implicit def M:MonadPlus[M]
  implicit def A:Arrow[=>:]

  def aempty[B,C](implicit A:Arrow[=>:]):B =>: C = kleisli(_ => M.empty)
  def aplus[B,C](f:B =>: C, g:B =>: C)(implicit A:Arrow[=>:]):B =>: C = kleisli(b => M.plus(f(b), g(b)))
}

object KleisliArrow {
  type K[M[_]] = {type t[a,b] = Kleisli[M,a,b]}

  implicit def kleisliArrowLoop[M[_]](implicit F0:Monad[M], MF0:MonadFix[M], A0:Arrow[K[M]#t]):ArrowLoop[K[M]#t] = 
    new KleisliArrowLoopInstance[M] {
      implicit def F:Monad[M] = F0
      implicit def MF:MonadFix[M] = MF0
      implicit def A:Arrow[K[M]#t] = A0
    }

  implicit def kleisliArrowPlus[M[_]](implicit M0:MonadPlus[M], A0:Arrow[K[M]#t]):ArrowPlus[K[M]#t] = 
    new KleisliArrowPlusInstance[M] {
      implicit def M:MonadPlus[M] = M0
      implicit def A:Arrow[K[M]#t] = A0
    }
}

// -----------------------------------------------------------------------
// Test it!
// -----------------------------------------------------------------------
object Test {
  import MonadFixOption._
  import KleisliArrow._

  type K[A,B] = Kleisli[Option,A,B]

  def kfact(implicit L:ArrowLoop[K]):K[Int,Int] = {
    L.loop[Int, Int, Int => Int](kleisli(bd => Monad[Option].point(lazyTuple2(bd._2(bd._1), {
      case 0 => 1
      case x => x * bd._2(x - 1)
    }))))
  }

  def main(args:Array[String]) {
    1 to 10 map kfact.run foreach println
  }
}
