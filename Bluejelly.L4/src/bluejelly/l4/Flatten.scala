/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.l4

import bluejelly.asm.AllocApp
import bluejelly.asm.AllocNapp
import bluejelly.asm.AllocTyCon
import bluejelly.asm.Block
import bluejelly.asm.Catch
import bluejelly.asm.Enter
import bluejelly.asm.EvalVar
import bluejelly.asm.Function
import bluejelly.asm.Instr
import bluejelly.asm.MatchChr
import bluejelly.asm.MatchCon
import bluejelly.asm.MatchDbl
import bluejelly.asm.MatchInt
import bluejelly.asm.MatchStr
import bluejelly.asm.MkApp
import bluejelly.asm.MkNapp
import bluejelly.asm.MkTyCon
import bluejelly.asm.PackApp
import bluejelly.asm.PackNapp
import bluejelly.asm.PackTyCon
import bluejelly.asm.PushChr
import bluejelly.asm.PushCode
import bluejelly.asm.PushCont
import bluejelly.asm.PushDbl
import bluejelly.asm.PushInt
import bluejelly.asm.PushStr
import bluejelly.asm.PushVar
import bluejelly.asm.Raise
import bluejelly.asm.RetChr
import bluejelly.asm.RetCon
import bluejelly.asm.RetDbl
import bluejelly.asm.RetInt
import bluejelly.asm.RetStr
import bluejelly.asm.Return
import bluejelly.asm.Slide
import bluejelly.asm.StackCheck
import bluejelly.utils.St.ret
import bluejelly.utils.St.upd
import bluejelly.utils.State

/**
 * Final pass of the L4 compiler. We do two things here:
 * 
 * <ol>
 * <li>Compute maximum stack depth and generate stack instructions</li>
 * <li>Flatten Reduce blocks, generating continuations and PushConts</li>
 * </ol>
 * 
 * <p>It's perfectly possible to use the resolve phase to compute max
 * stack depth and generating a stack instruction. But alas, peephole
 * optimizations might eliminate pushs, thus reducing the actual depth.
 * 
 * <p>If we want to avoid asking for more stack than we actually need, we
 * must defer max stack depth computation until code was fully optimized.
 * 
 * @author ppedemon
 */
class Flatten {
  import bluejelly.utils.State
  import bluejelly.utils.St._
  
  // ---------------------------------------------------------------------
  // Add stack check instructions to functions and alternatives
  // ---------------------------------------------------------------------
  
  // Our state is simply a pair holding current and maximum stack depth
  type S = (Int,Int)
  
  // Avoid long type signatures
  type A[T] = bluejelly.asm.Alt[T]
  type M = bluejelly.asm.Module
  
  private def push(n:Int):State[S,Unit] = upd {case (d,m) => (d+n, math.max(d+n,m))}
  private def pop(n:Int):State[S,Unit] = upd {case (d,m) => (d-n, m)}
  
  // Stack effect for single instructions
  private def effect(i:Instr):State[S,Unit] = i match {
    case Enter     | Return    | Raise     | Catch     => pop(1)
    case RetInt(_) | RetDbl(_) | RetChr(_) | RetStr(_) => push(1)
    case EvalVar(_,_) => push(1)
    case RetCon(_,n)  => pop(n-1)
    
    case PushVar(_) | PushCode(_) | PushInt(_) => push(1)
    case PushDbl(_) | PushChr(_)  | PushStr(_) => push(1)
    case Slide(_,m) => pop(m)
    
    case MkApp(n)             => pop(n-1)
    case MkNapp(n)            => pop(n-1)
    case AllocApp | AllocNapp => push(1)
    case PackApp(_,n)         => pop(n)
    case PackNapp(_,n)        => pop(n)
    
    case MkTyCon(_,n)   => pop(n-1)
    case AllocTyCon(_)  => push(1)
    case PackTyCon(_,n) => pop(n)
    
    case _ => ret()
  }

  // Process a single instruction
  private def process(i:Instr):State[S,Instr] = i match {
    case MatchCon(as,mb) => 
      ret(processMatch(MatchCon(_:List[A[Int]],_), as, mb))
    case MatchInt(as,mb) => 
      ret(processMatch(MatchInt(_:List[A[Int]],_), as, mb))
    case MatchDbl(as,mb) => 
      ret(processMatch(MatchDbl(_:List[A[Double]],_), as, mb))
    case MatchChr(as,mb) => 
      ret(processMatch(MatchChr(_:List[A[Char]],_), as, mb))
    case MatchStr(as,mb) => 
      ret(processMatch(MatchStr(_:List[A[String]],_), as, mb))
    
    case Reduce(n,m,b) => for {is <- process(b.is)} yield Reduce(n,m,is)
      
    case _ => for {_ <- effect(i)} yield i
  }
  
  // Process a sequence of instructions
  private def process(is:List[Instr]):State[S,List[Instr]] = is match {
    case Nil => ret(Nil)
    case i::is => for {
      i0 <- process(i)
      is0 <- process(is)
    } yield (i0::is0)
  }
  
  // Process match instructions
  private def processAlt[T](alt:A[T]):A[T] = 
    new A(alt.v, Block(addStackCheck(alt.b.is)))
  private def processDef(mb:Option[Block]):Option[Block] = 
    mb map (b => Block(addStackCheck(b.is)))
  private def processMatch[T](
      f:(List[A[T]],Option[Block]) => Instr, 
      as:List[A[T]], 
      mb:Option[Block]) = f(as map processAlt, processDef(mb))
  
  // Add stack check to the given instruction sequence
  private def addStackCheck(is:List[Instr]):List[Instr] = {
    val (is0,(_,m)) = process(is)(0,0)
    if (m > 0) StackCheck(m) :: is0 else is0
  }
  
  // Add stack check to a function
  private def addStackCheck(f:Function):Function = 
    new Function(f.name, f.arity, f.matcher, Block(addStackCheck(f.b.is)))

  // Add stack checks for all the functions in the given module
  private def addStackCheck(m:M):M = 
    new M(m.name, m.funcs map addStackCheck)
  
  // ---------------------------------------------------------------------
  // Real flattening starts here: get rid of Reduce blocks
  // ---------------------------------------------------------------------
  
  def flatten(m:M):M = new M(m.name, m.funcs flatMap flatten)
  
  private def flatten(f:Function):List[Function] = accum(f, f.b.is, Nil)
  
  private def accum(
      f:Function,
      in:List[Instr],
      out:List[Instr]):List[Function] = in match {
    case Nil => 
      List(new Function(f.name, f.arity, f.matcher, Block(out reverse)))
    case Reduce(n,m,b)::is => 
      val k = new Function(n,0,m,null)
      accum(f, b.is, cont(n,b.is,out)) ::: accum(k, is, Nil)
    case i::is =>
      accum(f, is, i::out)
  }
  
  // Compute output instructions for a function with a reduce block
  // whole instructions are is. If is is just an EvalVar, we don't
  // need to push a continuation after the block evaluation, since
  // the EvalVar instruction does it for us.
  private def cont(
      n:String, 
      is:List[Instr], 
      out:List[Instr]):List[Instr] = is match {
    case List(EvalVar(_,_)) => out
    case _ => PushCont(n) :: out
  }
}

object Flatten {
  def flatten(m:bluejelly.asm.Module) = {
    val obj = new Flatten
    obj flatten (obj addStackCheck m)
  }
}
