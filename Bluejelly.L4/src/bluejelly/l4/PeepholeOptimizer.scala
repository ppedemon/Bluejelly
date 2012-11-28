/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.l4

import bluejelly.asm.Block
import bluejelly.asm.Enter
import bluejelly.asm.EvalVar
import bluejelly.asm.Function
import bluejelly.asm.Instr
import bluejelly.asm.Jump
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
import bluejelly.asm.PushDbl
import bluejelly.asm.PushInt
import bluejelly.asm.PushStr
import bluejelly.asm.PushVar
import bluejelly.asm.RetChr
import bluejelly.asm.RetCon
import bluejelly.asm.RetDbl
import bluejelly.asm.RetInt
import bluejelly.asm.RetStr
import bluejelly.asm.Return
import bluejelly.asm.Slide
import bluejelly.utils.Name

/**
 * Optimize a sequence of assembler instructions.
 * @author ppedemon
 */
class PeepholeOptimizer(env:Env) {

  type M = bluejelly.asm.Module
  def optimize(m:M) = new M(m.name, m.funcs map optimizeFun)
  
  def optimizeFun(f:Function):Function = {
    val is = optimize (optimize (optimize(f.b.is)))
    new Function(f.name, f.arity, f.matcher, Block(is))
  }
  
  def optimize(is:List[Instr]):List[Instr] = is match {
    
    case Nil => Nil
    
    // These can be removed
    case MkApp(1)   :: is => optimize(is)
    case MkNapp(1)  :: is => optimize(is)
    case Slide(_,0) :: is => optimize(is)
    
    // Remove superfluous PushVars
    case PushVar(0) :: Slide(1,m) :: is if m >= 1 => 
      optimize (Slide(1,m-1) :: is)
    case PushVar(1) :: PushVar(1) :: Slide(2,m) :: is if m >= 2 => 
      optimize (Slide(2,m-2) :: is)
    case PushVar(2) :: PushVar(2) :: PushVar(2) :: Slide(3,m) :: is if m >= 3 =>
      optimize (Slide(3,m-3) :: is)
      
    // If you app if going to be evaluated right away, don't build it!
    case MkApp(x) :: Slide(n,m) :: Enter :: is =>
      Slide(n+x-1, m) :: Enter :: optimize(is)
    case MkNapp(x) :: Slide(n,m) :: Enter :: is =>
      Slide(n+x-1, m) :: Enter :: optimize(is)
    
    // Return in registers if possible
    case MkTyCon(t,n) :: Slide(1,m) :: Enter :: is =>
      Slide(n,m) :: RetCon(t,n) :: optimize(is)
    case PushInt(i) :: Slide(1,m) :: Enter :: is =>
      Slide(0,m) :: RetInt(i) :: optimize(is)
    case PushDbl(d) :: Slide(1,m) :: Enter :: is =>
      Slide(0,m) :: RetDbl(d) :: optimize(is)
    case PushChr(c) :: Slide(1,m) :: Enter :: is =>
      Slide(0,m) :: RetChr(c) :: optimize(is)
    case PushStr(s) :: Slide(1,m) :: Enter :: is =>
      Slide(0,m) :: RetStr(s) :: optimize(is)
      
    // The idea is that the Return instructions terminates a function
    // that reduces to whnf. Return in registers optimization aside,
    // this is the only instruction that leaves a whnf on the stack.
    case PackTyCon(off,n) :: Slide(1,m) :: Enter :: is =>
      PackTyCon(off,n) :: Slide(1,m) :: Return :: optimize(is)
  
    // Handle function calls. Any optimizations here depend on having
    // f's arity at hand, so we might lose optimization opportunities
    // for external functions whose arity was not declared (by means 
    // of the ``extern'' declaration) in the module being compiled.
    case PushCode(f) :: is => optimizeCall(f, optimize(is))
    
    // Optimize Reduce blocks evaluating a single var
    case Reduce(n,m,b) :: is => 
      Reduce(n,m,optimizeEval(n,optimize(b.is))) :: optimize(is)
      
    // Merge slides
    case Slide(n0,m0) :: Slide(n1,m1) :: is if n1 <= n0 =>
      optimize(Slide(n1, m0 + m1 - n0 + n1) :: is)
      
    case MatchCon(as,mb) :: is => 
      optimizeMatch(MatchCon(_:List[A[Int]],_), as, mb, is)
    case MatchInt(as,mb) :: is => 
      optimizeMatch(MatchInt(_:List[A[Int]],_), as, mb, is)
    case MatchDbl(as,mb) :: is => 
      optimizeMatch(MatchDbl(_:List[A[Double]],_), as, mb, is)
    case MatchChr(as,mb) :: is => 
      optimizeMatch(MatchChr(_:List[A[Char]],_), as, mb, is)
    case MatchStr(as,mb) :: is => 
      optimizeMatch(MatchStr(_:List[A[String]],_), as, mb, is)

    case i :: is => i :: optimize(is)
  }
  
  private def optimizeCall(f:String, is:List[Instr]) = {
    val v = toVar(f)
    if (env hasFun(v)) {
      val arity = env(v).args.length
      is match {
        case MkApp(n) :: is if arity >= n => 
          PushCode(f) :: MkNapp(n) :: is
        case PackApp(off,n) :: is if arity >= n => 
          PushCode(f) :: PackNapp(off,n) :: is
        case Slide(n,m) :: Enter :: is if arity == n-1 && arity > 0 =>
          Slide(n-1,m) :: Jump(f) :: is
        case _ => 
          PushCode(f) :: is
      }
    } else {
      PushCode(f) :: is
    }
  }
  
  // Precondition: is was already optimized
  private def optimizeEval(cont:String, is:List[Instr]) = is match {
    case List(PushVar(x),Enter) => List(EvalVar(x,cont))
    case List(PushVar(x),Slide(1,0),Enter) => List(EvalVar(x,cont))
    case _ => is
  }
  
  private def toVar(s:String) = {
    val ix = s.lastIndexOf('.')
    if (ix == -1) 
      new Var(Name(s)) 
    else 
      new Var(Name(s.substring(0,ix),s.substring(ix+1)))
  }
  
  type A[T] = bluejelly.asm.Alt[T]
  
  private def optimizeAlt[T](is:List[Instr])(alt:A[T]) = 
    new A(alt.v, Block(optimize(alt.b.is ::: is)))
  
  private def optimizeDef(mb:Option[Block], is:List[Instr]) = 
    mb map (b => Block(optimize(b.is ::: is)))
  
  private def optimizeMatch[T](
      f:(List[A[T]],Option[Block]) => Instr,
      alts:List[A[T]], 
      mb:Option[Block], 
      is:List[Instr]) = List(f(alts map optimizeAlt(is), optimizeDef(mb,is)))
}

object PeepholeOptimizer {
  def optimize(env:Env,m:bluejelly.asm.Module) = 
    new PeepholeOptimizer(env) optimize m
}
