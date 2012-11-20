/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.l4

import bluejelly.asm.Function
import bluejelly.asm.Instr
import bluejelly.asm.Block

/**
 * Resolve symbolic instructions to stack offsets.
 * @author ppedemon
 */
class Resolver {
  import bluejelly.utils.State
  import bluejelly.utils.St._
  
  // State:
  //  1. Map from variables to offset from bottom of the stack
  //  2. Current stack depth, measured from the bottom
  
  type S = (Map[Var,Int],Int)
  
  /* A little ASCII art explaining:
   * 
   *   Bottom of the stack
   *    +--------------+
   *  1 |      z       |
   *    +--------------+
   *  2 |      y       |
   *    +--------------+
   *    |     ...      |
   *    +--------------+
   *  n |      x       |
   *    +--------------+
   *    
   *  Then, for the state (m,d) reflecting this configuration:
   *    - m(z) = 0, m(y) = 2, m(x) = n
   *    - d = n
   *    - offset(v) = d - m(v), offset(v) computes offset of
   *        variable v from the *top* of the stack
   */
  
  // ---------------------------------------------------------------------
  // Utility functions
  // ---------------------------------------------------------------------
  
  def push(n:Int):State[S,Unit] = upd {case (m,d) => (m,d+n)}
  def pop(n:Int) = push(-n)
  def depth():State[S,Int] = for {s <- get} yield s._2
  def bind[A](v:Var,f:State[S,A]):State[S,A] = state{case (m,d) => f(m + (v->d),d)}
  def offset(v:Var):State[S,Int] = for {s <- get} yield s._2 - s._1(v)
  
  def alt[A](d:Int,f:State[S,A]):State[S,A] = state {
    case s@(m,_) =>
      val (x,(_,d1)) = f(s)
      assert(d1 == d+1, "resolver[match alt]: invalid stack")
      (x,(m,d1))
  }
  
  def run[A](f:State[S,A]):A = {
    val (x,(_,d)) = f((Map(),0))
    assert(d == 0, "resolver[run]: invalid stack")
    x
  }
  
  // ---------------------------------------------------------------------
  // Solve instructions
  // ---------------------------------------------------------------------

  def resolve(f:Function):Function = {
    val instrs = run(resolve(f.b.is))
    new Function(f.name, f.arity, f.matcher, Block(instrs))
  }
  
  def resolve(instrs:List[Instr]):State[S,List[Instr]] = instrs match {
    case Nil => ret(Nil)
    case Param(v)::is => for {_ <- push(1); rs <- bind(v,resolve(is))} yield rs
    case Local(v)::is => bind(v,resolve(is))
    case i::is => for {r <- resolve(i); rs <- resolve(is)} yield r:::rs
  }
  
  // TODO: Implement me!
  def resolve(i:Instr):State[S,List[Instr]] = null

}