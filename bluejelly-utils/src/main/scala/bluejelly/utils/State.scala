/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */

package bluejelly.utils

/**
 * State monad. This is one possible Scala idiom for the well known
 * Haskell declaration:  
 *   
 *   <code>data ST s a = ST {unSt :: s -> (a,s)}</code>
 *   
 * <p>The unST function is modeled by the [[apply]] method. In Scala, the
 * usual way to define a monad is by implementing [[map]] and [[flatMap]],
 * and so we implement them here. The [[return]] operation is provided
 * automatically by Scala's [[yield]] construction.
 */
trait State[S,A] {  
  import St._
  def apply(s:S):(A,S)
  def map[B](f:A => B):State[S,B] = state(this(_) match {
    case (a,s) => (f(a),s)
  })
  def flatMap[B](f:A => State[S,B]):State[S,B] = state(this(_) match {
    case (a,s) => f(a)(s) 
  })
}

/**
 * Companion object for the state monad. We provide a handful of utility
 * functions here, namely:
 * <ol>
 * <li> [[state]] convenience factory for monad objects, used in the monad itself
 * <li> [[ret]] explicit implementation of the [[return]] operation
 * <li> [[get]] get current state
 * <li> [[upd]] mutate state
 * </ol>
 */
object St {
  def state[S,A](f:S => (A,S)) = new State[S,A] { def apply(s:S) = f(s) }
  def ret[S,A](a:A):State[S,A] = state(s => (a,s))
  def get[S]:State[S,S] = state(s => (s,s))
  def upd[S](f:S => S):State[S,Unit] = get flatMap (s => state(_ => ((),f(s))))
  
  def seq[S,A](xs:List[State[S,A]]):State[S,List[A]] = xs match {
    case Nil => ret(Nil)
    case x::xs => for {
      a <- x
      as <- seq(xs)
    } yield a::as
  }
}
