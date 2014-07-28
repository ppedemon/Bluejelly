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
import bluejelly.asm.PushVar
import bluejelly.asm.PackApp
import bluejelly.asm.PackNapp
import bluejelly.asm.PackTyCon
import bluejelly.asm.Slide
import bluejelly.asm.MatchInt
import bluejelly.asm.MatchInt
import bluejelly.asm.MatchDbl
import bluejelly.asm.MatchDbl
import bluejelly.asm.MatchChr
import bluejelly.asm.MatchChr
import bluejelly.asm.MatchStr
import bluejelly.asm.MatchCon
import bluejelly.asm.Enter
import bluejelly.asm.Raise
import bluejelly.asm.Catch
import bluejelly.asm.PushInt
import bluejelly.asm.PushDbl
import bluejelly.asm.PushChr
import bluejelly.asm.PushStr
import bluejelly.asm.PushCode
import bluejelly.asm.MkApp
import bluejelly.asm.MkNapp
import bluejelly.asm.AllocApp
import bluejelly.asm.AllocNapp
import bluejelly.asm.MkTyCon
import bluejelly.asm.AllocTyCon
import bluejelly.asm.StackCheck
import bluejelly.asm.MatchCon

/**
 * Resolve symbolic instructions to stack offsets.
 * @author ppedemon
 */
class Resolver {
  import bluejelly.utils.State
  import bluejelly.utils.St._

  // Avoid verbose types in definitions
  type M = bluejelly.asm.Module
  type A[T] = bluejelly.asm.Alt[T]
  
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
  def bind[A](v:Var,f:State[S,A]):State[S,A] = state {case (m,d) => f(m + (v->d),d)}
  def offset(v:Var):State[S,Int] = for {s <- get} yield s._2 - s._1(v)
  
  def alt[A](d:Int,f:State[S,A]):State[S,A] = state {
    case s@(m,_) =>
      val (x,(_,d1)) = f((m,d))
      assert(d1 == d+1, "resolver[match alt]: invalid stack, height = %s, expected = %s" format (d1,d+1))
      (x,(m,d1))
  }
  
  def run[A](f:State[S,A]):A = {
    val (x,(_,d)) = f((Map(),0))
    assert(d == 0, "resolver[run]: invalid stack, height = %d (expected 0)" format d)
    x
  }
  
  // ---------------------------------------------------------------------
  // Solve instructions
  // ---------------------------------------------------------------------

  def resolve(m:M):M = new M(m.name, m.funcs map resolve)
  
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
  
  def resolve(i:Instr):State[S,List[Instr]] = i match {
    case PushSym(v) => for {
      x <- offset(v)
      _ <- push(1)
    } yield List(PushVar(x))
    
    case PackAppSym(v,n) => for {
      x <- offset(v)
      _ <- pop(n)
    } yield List(PackApp(x-n,n))
    
    case PackNappSym(v,n) => for {
      x <- offset(v)
      _ <- pop(n)
    } yield List(PackNapp(x-n,n))
    
    case PackTyConSym(v,n) => for {
      x <- offset(v)
      _ <- pop(n)
    } yield List(PackTyCon(x-n,n))
    
    case Reduce(n,m,b) => for {
      is <- resolve(b.is)
      _  <- push(1) 
    } yield List(Reduce(n,m,Block(is)))
    
    case Atom(block) => resolveSlide(1,block.is)
    case Init(block) => resolveSlide(0,block.is)
    
    case MatchCon(alts,deflt) => 
      resolveMatch(alts,deflt,MatchCon(_:List[A[Int]],_))
    case MatchInt(alts,deflt) => 
      resolveMatch(alts,deflt,MatchInt(_:List[A[Int]],_))
    case MatchDbl(alts,deflt) => 
      resolveMatch(alts,deflt,MatchDbl(_:List[A[Double]],_))
    case MatchChr(alts,deflt) => 
      resolveMatch(alts,deflt,MatchChr(_:List[A[Char]],_))
    case MatchStr(alts,deflt) => 
      resolveMatch(alts,deflt,MatchStr(_:List[A[String]],_))
      
    case i => for {_ <- effect(i)} yield List(i)
  }

  /*
   * Solve a generic match instruction, function f builds the intended
   * match instructions given the alternatives and the default block. 
   */
  def resolveMatch[T](
      alts:List[A[T]],
      deflt:Option[Block],
      f:(List[A[T]],Option[Block])=>Instr):State[S,List[Instr]] = for { 
    _  <- pop(1)
    d  <- depth()
    as <- resolveAlts(d,alts)
    mb <- resolveDef(d, deflt)
  } yield List(f(as,mb))
    
  def resolveSlide(n:Int, instrs:List[Instr]):State[S,List[Instr]] = for {
    d0 <- depth()
    is <- resolve(instrs)
    d1 <- depth()
    m = d1 - d0 - n
    _ <- pop(m)
  } yield is ::: List(Slide(n,m))
  
  // ---------------------------------------------------------------------
  // Solve match instructions
  // ---------------------------------------------------------------------  
  def resolveAlts[T](d:Int, alts:List[A[T]]):State[S,List[A[T]]] = for {
    as <- seq(alts map resolveAlt(d))
  } yield as

  def resolveAlt[T](d:Int)(a:A[T]):State[S,A[T]] = for {
    is <- alt(d,resolve(a.b.is))
  } yield new A(a.v, Block(is))
 
  def resolveDef(d:Int, mb:Option[Block]):State[S,Option[Block]] = mb match {
    case None => ret(None)
    case Some(b) => for {
      is <- alt(d,resolve(b.is))
    } yield Some(Block(is))
  }

  // ---------------------------------------------------------------------
  // List of stack effects for simple instructions
  // ---------------------------------------------------------------------
  
  /*
   * Just include the instructions generated by the compiler that
   * weren't handled before by resolve(i:Instr). If anything else
   * appears, we know something is wrong.
   */
  def effect(i:Instr):State[S,Unit] = i match {
    case Enter => pop(1)
    case Raise => pop(1)  // Really?
    case Catch => pop(1)
    
    case PushInt(_) => push(1)
    case PushDbl(_) => push(1)
    case PushChr(_) => push(1)
    case PushStr(_) => push(1)
    case PushCode(_) => push(1)
    
    case MkApp(n) => for {_ <- pop(n); _ <- push(1)} yield ()
    case MkNapp(n) => for {_ <- pop(n); _ <- push(1)} yield ()
    case AllocApp => push(1)
    case AllocNapp => push(1)
    
    case MkTyCon(_,n) => for {_ <- pop(n); _ <- push(1)} yield ()
    case AllocTyCon(_) => push(1)
    
    case _ => assert(false, "Unexpected instruction: " + i); ret()
  }
}

object Resolver {
  def resolve(m:bluejelly.asm.Module) = new Resolver resolve m
}
