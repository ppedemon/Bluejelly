package bluejelly.bjc.monad

import scalaz._
import std.option._
import syntax.monad._

trait MonadFix[M[_]] {
  def mfix[A](f: (=> A) => M[A])(implicit M:Monad[M]):M[A]
}

// -----------------------------------------------------------------------
// Add instance for the Option[_] type
// -----------------------------------------------------------------------
sealed class OptionMonadFixInstance extends MonadFix[Option] {
  implicit def M:Monad[Option] = Monad[Option]

  def mfix[A](f: (=> A) => Option[A])(implicit M:Monad[Option]):Option[A] = 
    new { val a:Option[A] = f(a.fold(sys.error("mfix Option: None"))(x => x)) }.a
}

object MonadFixOption {
  implicit def monadFixOption:MonadFix[Option] = new OptionMonadFixInstance
}

// -----------------------------------------------------------------------
// Test it!
// -----------------------------------------------------------------------
object MonadFixTest {
  import MonadFixOption._

  def mfact(x:Int)(implicit MF:MonadFix[Option]):Option[Int] = {
    val rec:(=> (Int => Int)) => Option[Int => Int] = f => Monad[Option].point(x => if (x == 0) 1 else x*f.apply(x-1))
    MF.mfix(rec) >>= (f => Monad[Option].point(f(x)))
  }

  def main(args:Array[String]) {
    1 to 10 map mfact foreach println
  }
}
