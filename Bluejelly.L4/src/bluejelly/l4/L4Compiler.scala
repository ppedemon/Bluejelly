/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.l4

import bluejelly.utils.Name
import bluejelly.asm.Function
import bluejelly.asm.Block
import bluejelly.asm.Instr
import bluejelly.asm.PushInt
import bluejelly.asm.PushDbl
import bluejelly.asm.PushChr
import bluejelly.asm.PushStr
import bluejelly.asm.Enter
import scala.text._
import scala.text.Document._
import java.io.StringWriter

/**
 * Compile a L4 module to an Assembler module.
 * @author ppedemon
 */
class L4ToAsm(val m:Module, val env:Env) {

  def compile:bluejelly.asm.Module = {
    val fc = new FunCompiler(env)
    val n = m.n.toString
    val funs = compileFuns(m.decls, fc)
    new bluejelly.asm.Module(n,funs)
  }
  
  def compileFuns(ds:List[Decl], fc:FunCompiler):List[Function] = ds match {
    case Nil => Nil
    case DataDecl(_,_)::ds => compileFuns(ds,fc)
    case (f@FunDecl(_,_,_))::ds => (fc compile f)::compileFuns(ds,fc)
  }
}

/**
 * Compile a single top level function.
 * @author ppedemon
 */
private class FunCompiler(env:Env) {
  
    // Let's use some state... I don't to pass this around
    var currId:Int = 0
    var currName:Var = null
  
    // TODO: implement
    def compile(fun:FunDecl):Function = {
      currName = fun.n
      currId = 1
      new Function(fun.n.n.toString, 0, false, new Block(List()))
    }

    // TODO: complete
    def compileExpr(expr:Expr):List[Instr] = expr match {      

      // Optimized eval cases
      case Eval(x, Note(Once,e), Match(y,alts)) if x == y && isWhnf(e) =>
        Atom(new Block(compileExpr(e))) :: compileAlts(alts)
      case Eval(x, Note(Once,e), Match(y,alts)) if x == y =>
        Reduce :: Atom(new Block(compileExpr(e))) :: 
          Cont(fresh(env),true) :: compileAlts(alts)
      case Eval(x,e,b) if isWhnf(e) => 
        Atom(new Block(compileExpr(e))) :: Local(x) :: compileExpr(b)
      case Eval(x,e,App(y,Nil)) if x == y => compileExpr(e)
      
      // The rest
      case LetRec(decls,e) => compileDecls(decls) ::: compileExpr(e)
    }

    // TODO: implement
    def compileAlts(alts:List[Alt]):List[Instr] = List()

    // TODO: implement
    def compileDecls(decls:List[(Var,Expr)]):List[Instr] = List()
    
    // TODO: complete
    def compileAtom(expr:Expr):List[Instr] = expr match {
      case ELit(lit) => List(compileLit(lit))  
    }

    def compileLit(lit:Lit):Instr = lit match {
      case IntLit(i) => PushInt(i)
      case DblLit(d) => PushDbl(d)
      case ChrLit(c) => PushChr(c)
      case StrLit(s) => PushStr(s)
    }
              
    def isWhnf(expr:Expr):Boolean = expr match {
      case ELit(_)  | ECon(_,_) => true
      case App(_,_) | NApp(_,_) => false
      case Let(_,_,e)  => isWhnf(e)
      case Eval(_,_,e) => isWhnf(e)
      case LetRec(_,e) => isWhnf(e)
      case Match(_,alts) => alts forall (a => isWhnf(a.e))
      case Note(_,e) => isWhnf(e)
    }
    
    def fresh(env:Env):String = {
      val s = Name(currName + "$" + currId)
      if (env hasFun (new Var(s))) {
        currId += 1
        fresh(env) 
      } else s toString
    }
}
