package bluejelly.bjc.monad

import scalaz._
import std.tuple._
import syntax.associative._
import syntax.monad._

import SupplyT.{SupplyTF,StateTF}

final case class SupplyT[F[_],A](st:StateT[F,Int,A]) {
  def map[B](f:A => B)(implicit F:Functor[F]):SupplyT[F,B] = SupplyT[F,B](st map f)
  
  def flatMap[B](f:A => SupplyT[F,B])(implicit F:Monad[F]):SupplyT[F,B] = SupplyT[F,B](st flatMap (f(_).st))

  def run(initial:Int)(implicit F:Monad[F]):F[(Int,A)] = st.run(initial)

  def eval(initial:Int)(implicit F:Monad[F]):F[A] = st.eval(initial)
}

sealed abstract class SupplyTInstances {
  implicit def supplyTMonadTrans:Hoist[SupplyT] = new SupplyTHoist {}

  implicit def supplyTMonadError[F[_],E](implicit F0:MonadError[F,E]):MonadError[SupplyTF[F]#t,E] = 
    new SupplyTMonadError[F,E] {
      implicit def F:MonadError[F,E] = F0
    }

  implicit def supplyTMonadListen[F[_],W](implicit F0:MonadListen[F,W]):MonadListen[SupplyTF[F]#t,W] = 
    new SupplyTMonadListen[F,W] {
      implicit def F:MonadListen[F,W] = F0
    }

  implicit def supplyTMonadReader[F[_],E](implicit F0:MonadReader[F,E]):MonadReader[SupplyTF[F]#t,E] = 
    new SupplyTMonadReader[F,E] {
      implicit def F:MonadReader[F,E] = F0
    }

  implicit def supplyTMonadSupply[F[_]](implicit F0:Monad[F]):MonadSupply[SupplyTF[F]#t] = 
    new SupplyTMonadSupply[F] {
      implicit def F:Monad[F] = F0
    }
}

object SupplyT extends SupplyTInstances {
  private[monad] type SupplyTF[F[_]] = {
    type t[x] = SupplyT[F,x]
  }

  private[monad] type StateTF = {
    type t[f[_],x] = StateT[f,Int,x]
  }

  import Id._
  type Supply[A] = SupplyT[Id,A]

  def supplyT[F[_]:Monad,A](a:A):SupplyT[F,A] = SupplyT[F,A](StateT.stateT[F,Int,A](a))
}

private trait SupplyTMonad[F[_]] extends Monad[SupplyTF[F]#t] {
  implicit def F:Monad[F]
  
  def point[A](a: =>A):SupplyT[F,A] = SupplyT[F,A](StateT.stateT[F,Int,A](a))
  
  override def bind[A,B](s:SupplyT[F,A])(f:A => SupplyT[F,B]):SupplyT[F,B] = s flatMap f  
}

private trait SupplyTHoist extends Hoist[SupplyT] {
  def liftM[G[_],A](ga:G[A])(implicit G:Monad[G]):SupplyT[G, A] = 
    SupplyT(ga.liftM[StateTF#t])

  implicit def apply[G[_]:Monad]:Monad[SupplyTF[G]#t] = SupplyT.supplyTMonadSupply

  def hoist[M[_]:Monad,N[_]](f: M ~> N) = new (SupplyTF[M]#t ~> SupplyTF[N]#t) {
    def apply[A](action:SupplyT[M,A]) = SupplyT[N,A](IndexedStateT.createState(
      (n:Monad[N]) => (s:Int) => f(action.st(s))))
  }
}

private trait SupplyTMonadError[F[_],E] extends MonadError[SupplyTF[F]#t,E] 
    with SupplyTMonad[F] 
    with SupplyTHoist {
  
  implicit def F:MonadError[F,E]
  
  override def raiseError[A](e: E) = liftM[F,A](F.raiseError[A](e))
  
  override def handleError[A](fa:SupplyT[F,A])(f:E => SupplyT[F,A]) =
    SupplyT[F,A](StateT(s => F.handleError(fa.st.run(s))(f(_).st.run(s))))
}

private trait SupplyTMonadTell[F[_],W] extends MonadTell[SupplyTF[F]#t,W] 
    with SupplyTMonad[F]
    with SupplyTHoist {
  
  implicit def F:MonadTell[F,W]
  
  def writer[A](w:W, v:A):SupplyT[F,A] = liftM[F,A](F.writer(w,v))
}

private trait SupplyTMonadListen[F[_],W] extends MonadListen[SupplyTF[F]#t,W] with SupplyTMonadTell[F,W] {
  implicit def F:MonadListen[F,W]
  
  def listen[A](fa:SupplyT[F,A]):SupplyT[F,(A,W)] = 
    SupplyT(StateT(s => F.map(F.listen(fa.st.run(s)))(_.reassociateRight[Int,A])))
}

private trait SupplyTMonadReader[F[_],E] extends MonadReader[SupplyTF[F]#t,E] 
    with SupplyTMonad[F]
    with SupplyTHoist {

  implicit def F:MonadReader[F,E]
  
  def ask:SupplyT[F,E] = liftM[F,E](F.ask)
  
  def local[A](f:E => E)(fa:SupplyT[F,A]):SupplyT[F,A] = SupplyT(fa.st.mapK(F.local(f)))
}

private trait SupplyTMonadSupply[F[_]] extends MonadSupply[SupplyTF[F]#t] with SupplyTMonad[F] {
  implicit def F:Monad[F]

  def fresh:SupplyT[F,Int] = SupplyT(StateT[F,Int,Int](s => F.point(s+1,s)))
}
