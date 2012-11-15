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
import bluejelly.asm.AllocApp
import bluejelly.asm.AllocNapp
import bluejelly.asm.AllocTyCon

import scala.text._
import scala.text.Document._
import java.io.StringWriter

/**
 * Compile a L4 module to an Assembler module.
 * @author ppedemon
 */
class L4Compiler(val m:Module, val env:Env) {

  def compile:bluejelly.asm.Module = {
    val fc = new FunCompiler()
    val n = m.n.toString
    val funs = compileFuns(m.decls, fc)
    new bluejelly.asm.Module(n,funs)
  }
  
  def compileFuns(ds:List[Decl], fc:FunCompiler):List[Function] = ds match {
    case Nil => Nil
    case DataDecl(_,_)::ds => compileFuns(ds,fc)
    case (f@FunDecl(_,_,_))::ds => (fc compile (env,f))::compileFuns(ds,fc)
  }
}

/**
 * Compile a single top level function.
 * @author ppedemon
 */
private class FunCompiler {
  
    // Let's use some state... I don't to pass this around
    var currId:Int = 0
    var currName:Var = null
  
    // TODO: implement
    def compile(env:Env,fun:FunDecl):Function = {
      currName = fun.n
      currId = 1
      new Function(fun.n.n.toString, 0, false, new Block(List()))
    }

    // TODO: complete
    def compileExpr(env:Env,expr:Expr):List[Instr] = expr match {      

      // Optimized eval cases
      case Eval(x, Note(Once,e), Match(y,alts)) if x == y && isWhnf(e) =>
        Atom(compileExpr(env,e)) :: compileAlts(env,alts)
      case Eval(x, Note(Once,e), Match(y,alts)) if x == y =>
        val block = Block(Atom(compileExpr(env,e)),Enter)
        Reduce(fresh(env),true,block) :: compileAlts(env,alts)
      case Eval(x,e,b) if isWhnf(e) => 
        Atom(compileExpr(env,e)) :: Local(x) :: compileExpr(env,b)
      case Eval(x,e,App(y,Nil)) if x == y => compileExpr(env,e)
      
      // The rest
      case LetRec(decls,e) => compileDecls(env,decls) ::: compileExpr(env,e)
    }

    // TODO: implement
    def compileAlts(env:Env,alts:List[Alt]):List[Instr] = List()

    // TODO: implement
    def compileDecls(env:Env,decls:List[(Var,Expr)]):List[Instr] = {
      decls map {case (v,e) => List(Atom(compileAlloc(env,e)),Local(v))} flatten
    }
    
    // Compile atom allocation for mutually recursive declarations
    def compileAlloc(env:Env,expr:Expr):List[Instr] = expr match {
      case App(_,_)     => List(AllocApp)
      case NApp(_,_)    => List(AllocNapp)
      case Let(_,_,e)   => compileAlloc(env,e)
      case LetRec(_,e)  => compileAlloc(env,e)
      case ECon(c,args) => List(AllocTyCon(env(c).con.tag))
      case Note(_,e)    => compileAlloc(env,e)
      case _            => l4Panic("unexpected binding in let rec",Nil)
    }
    
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
    
    def l4Panic[T](s:String,x:T):T = {
      assert(false,"l4c panic: " + s)
      x
    }
}
