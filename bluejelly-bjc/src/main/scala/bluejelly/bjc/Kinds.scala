package bluejelly.bjc

import scalaz._
import syntax.monad._

trait Kind
case class KUnknown(val n:Int) extends Kind
case object KStar extends Kind
case class KFun(val from:Kind, val to:Kind) extends Kind

object Kinds {
  def everywhereOnKinds(f:Kind => Kind)(k:Kind):Kind = {
    def go:Kind => Kind = {
      case KFun(k1,k2) => f(KFun(go(k1), go(k2)))
      case k => f(k)
    }
    go(k)
  }

  def everywhereOnKindsM[M[_]:Monad](f:Kind => M[Kind])(k:Kind):M[Kind] = {
    def go:Kind => M[Kind] = {
      case KFun(k1,k2) => ((go(k1) |@| go(k2))(KFun.apply _)) >>= f
      case k => f(k)
    }
    go(k)
  }

  def everythingOnKinds[A](f:(A,A) => A)(g:Kind => A)(k:Kind):A = {
    def go:Kind => A = {
      case k@KFun(k1,k2) => f(f(g(k), go(k1)), go(k2))
      case k => g(k)
    }
    go(k)
  }
}
