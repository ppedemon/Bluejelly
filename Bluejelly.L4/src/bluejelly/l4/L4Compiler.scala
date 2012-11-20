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
import bluejelly.asm.Enter
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
import bluejelly.asm.PushChr
import bluejelly.asm.PushCode
import bluejelly.asm.PushDbl
import bluejelly.asm.PushInt
import bluejelly.asm.PushStr
import bluejelly.utils.Name

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
    case (f@FunDecl(_,_,_))::ds => (fc compile (env,f)) :: compileFuns(ds,fc)
  }
}

/**
 * Compile a single top level function.
 * @author ppedemon
 */
private class FunCompiler {
  
    // Let's use some state... I don't want to pass this around
    var currId:Int = 0
    var currName:Var = null

    // Compile a function
    def compile(env:Env,fun:FunDecl):Function = {
      currName = fun.n
      currId = 1
      val params = (fun.args reverse) map {Param(_)}
      val instrs = compileExpr(env addLocals(fun.args))(fun.body)
      val block = Block(Atom(params ::: instrs),Enter)
      new Function(fun.n.n.toString, fun.args.length, false, block)
    }

    // -------------------------------------------------------------------
    // Compile expressions
    // -------------------------------------------------------------------
    def compileExpr(env:Env)(expr:Expr):List[Instr] = expr match {      

      // Optimized eval cases
      case Eval(x, Note(Once,e), Match(y,alts)) if x == y && isWhnf(e) =>
        Atom(compileExpr(env)(e)) :: compileAlts(env,alts)
        
      case Eval(x, Note(Once,e), Match(y,alts)) if x == y =>
        val block = Block(Atom(compileExpr(env)(e)),Enter)
        Reduce(fresh(env),true,block) :: compileAlts(env,alts)
      
      case Eval(x,e,b) if isWhnf(e) => 
        Atom(compileExpr(env)(e)) :: Local(x) :: compileExpr(env addLocal x)(b)
      
      case Eval(x,e,App(y,Nil)) if x == y => compileExpr(env)(e)
      
      // The rest
      case Let(x,e,b) => 
        compileBind(env,x,e) ::: compileExpr(env addLocal x)(b)
      
      case Eval(x,e,b) =>
        val block = Block(Atom(compileExpr(env)(e)),Enter)
        val extEnv = env addLocal x
        Reduce(fresh(env),false,block) :: Local(x) :: compileExpr(extEnv)(b)
      
      case LetRec(decls,e) => 
        val extEnv = env addLocals (decls map {_._1})
        compileDecls(extEnv,decls) ::: compileExpr(extEnv)(e)
      
      case Match(v,alts) => 
        compileVar(env,v) :: compileAlts(env,alts)
      
      case Note(_,e) => 
        compileExpr(env)(e)
        
      case _ => compileAtom(env)(expr)
    }
    
    // -------------------------------------------------------------------
    // Compile match alternatives
    // -------------------------------------------------------------------
    def compileAlts(env:Env,alts:List[Alt]):List[Instr] = {
      val (as,mdef) = partition(alts)
      val d = mdef map (a => compileDefAlt(env)(a.e,defVar(a)))
      val m = as.head.p match {
        case PLit(IntLit(_)) => MatchInt(compileIntAlts(env)(as), d)
        case PLit(DblLit(_)) => MatchDbl(compileDblAlts(env)(as), d)
        case PLit(ChrLit(_)) => MatchChr(compileChrAlts(env)(as), d)
        case PLit(StrLit(_)) => MatchStr(compileStrAlts(env)(as), d)
        case PCon(c,args )   => MatchCon(compileConAlts(env)(as), d)
        case PVar(_)         => l4Panic("unexpected variable alternative")
      }
      List(m)
    }
    
    // Get "normal" alternatives and default one, if present
    def partition(alts:List[Alt]):(List[Alt],Option[Alt]) = {
      val (defs,rest) = alts partition(_ isVarAlt)
      (rest, if (defs isEmpty) None else Some(defs.head))
    }

    // Get variable of default alternative
    def defVar(alt:Alt) = alt.p match {
      case PVar(v) => v
      case _ => l4Panic("invalid default alternative")
    }
    
    // Compile default alternative
    def compileDefAlt(env:Env)(e:Expr,v:Var) = 
      Block(Atom(Param(v) :: compileExpr(env)(e)))

    // Compile alternative to a block
    def compileAlt[T](env:Env)(
        v:T, e:Expr, 
        params:List[Var]=Nil):bluejelly.asm.Alt[T] = {
      val prev = params reverse
      val extEnv = env addLocals params
      val b = Block(Atom((prev map {Param(_)}) ::: compileExpr(extEnv)(e)))
      new bluejelly.asm.Alt(v, b)
    }
    
    def compileIntAlts(env:Env)(alts:List[Alt]) = alts map (a => a.p match {
      case PLit(IntLit(i)) => compileAlt(env)(i,a.e)
      case _ => l4Panic("expected int alternative")
    })
    
    def compileDblAlts(env:Env)(alts:List[Alt]) = alts map (a => a.p match {
      case PLit(DblLit(d)) => compileAlt(env)(d,a.e)
      case _ => l4Panic("expected double-precision alternative")
    })
    
    def compileChrAlts(env:Env)(alts:List[Alt]) = alts map (a => a.p match {
      case PLit(ChrLit(c)) => compileAlt(env)(c,a.e)
      case _ => l4Panic("expected char alternative")
    })
    
    def compileStrAlts(env:Env)(alts:List[Alt]) = alts map (a => a.p match {
      case PLit(StrLit(s)) => compileAlt(env)(s,a.e)
      case _ => l4Panic("expected string alternative")
    })
    
    def compileConAlts(env:Env)(alts:List[Alt]) = alts map (a => a.p match {
      case PCon(c,args) => compileAlt(env)(env(c).con.tag, a.e, args)
      case _ => l4Panic("expect datacon alternative")
    })
    
    // -------------------------------------------------------------------
    // Compile let and let recs bindings
    // -------------------------------------------------------------------
    def compileBind(env:Env, x:Var, e:Expr):List[Instr] =
      compileAtom(env)(e) ::: List(Local(x))

    def compileDecls(env:Env,decls:List[(Var,Expr)]):List[Instr] = {
      val as = decls map {
        case (v,e) => List(Atom(compileAlloc(env,e)),Local(v))
      }
      val is = decls map {
        case (v,e) => compileInit(env,v,e)
      }
      (as ::: is) flatten
    }

    // Compile atom allocation for mutually recursive declarations
    def compileAlloc(env:Env,expr:Expr):List[Instr] = expr match {
      case App(_,_)     => List(AllocApp)
      case NApp(_,_)    => List(AllocNapp)
      case Let(_,_,e)   => compileAlloc(env,e)
      case LetRec(_,e)  => compileAlloc(env,e)
      case ECon(c,args) => List(AllocTyCon(env(c).con.tag))
      case Note(_,e)    => compileAlloc(env,e)
      case _            => l4Panic("unexpected binding in let rec")
    }
    
    def compileInit(env:Env,id:Var,expr:Expr):List[Instr] = expr match {
      case App(v,args) => 
        val n = args.length + 1
        compileArgs(env,args) ::: compileVar(env,v) :: List(PackAppSym(id,n))
      case NApp(v,args) =>
        val n = args.length + 1
        compileArgs(env,args) ::: compileVar(env,v) :: List(PackNappSym(id,n))
      case LetRec(decls,e) =>
        val extEnv = env addLocals (decls map {_._1})
        compileDecls(extEnv,decls) ::: compileInit(extEnv,id,e)
      case Let(x,e,b) => 
        compileBind(env,x,e) ::: compileInit(env addLocal x,id,b)
      case ECon(c,args) => 
        compileArgs(env,args) ::: List(PackTyConSym(id,args.length))
      case Note(_,e) => 
        compileInit(env,id,e)
      case _ => 
        l4Panic("unexpected binding in let rec")
    }
    
    // -------------------------------------------------------------------
    // Compile atoms
    // -------------------------------------------------------------------
    def compileAtom(env:Env)(expr:Expr):List[Instr] = 
      List(Atom(compileAtomAux(env,expr)))
    
    def compileAtomAux(env:Env,expr:Expr):List[Instr] = expr match {
      case ELit(lit) => 
        List(compileLit(lit))
      case ECon(c,args) =>
        compileArgs(env,args) ::: List(MkTyCon(env(c).con.tag, args.length))
      case App(v,args) => 
        compileArgs(env,args) ::: 
        compileVar(env,v) :: 
        (if (args.isEmpty) Nil else List(MkApp(args.length + 1)))
      case NApp(v,args) =>
        compileArgs(env,args) ::: 
        compileVar(env,v) :: 
        (if (args.isEmpty) Nil else List(MkNapp(args.length + 1)))
      case Let(x,e,b) =>
        compileBind(env, x, e) ::: compileAtomAux(env addLocal x, b)
      case LetRec(decls,e) =>
        val extEnv = env addLocals (decls map {_._1})
        compileDecls(extEnv,decls) ::: compileAtomAux(extEnv,e)
      case Eval(x,e,b) if isWhnf(e) =>
        val extEnv = env addLocal x
        Atom(compileExpr(env)(e)) :: Local(x) :: compileAtomAux(extEnv, b)
      case Eval(x,e,b) =>
        val block = Block(Atom(compileExpr(env)(e)),Enter)
        val extEnv = env addLocal x
        Reduce(fresh(env),false,block) :: Local(x) :: compileAtomAux(extEnv, b)
      case Note(_,e) =>
        compileAtomAux(env,e)
      case _ =>
        l4Panic("unexpected non-atomic expression")
    }

    def compileArgs(env:Env,args:List[Expr]):List[Instr] = 
      (args reverse) map compileAtom(env) flatten
    
    def compileVar(env:Env, v:Var):Instr = 
      if (env isLocal v) PushSym(v) else PushCode(v.n.toString)
      
    def compileLit(lit:Lit):Instr = lit match {
      case IntLit(i) => PushInt(i)
      case DblLit(d) => PushDbl(d)
      case ChrLit(c) => PushChr(c)
      case StrLit(s) => PushStr(s)
    }

    // -------------------------------------------------------------------
    // Auxiliary
    // -------------------------------------------------------------------
    def isWhnf(expr:Expr):Boolean = expr match {
      case ELit(_)  | ECon(_,_) => true
      case App(_,_) | NApp(_,_) => false
      case Let(_,_,e)    => isWhnf(e)
      case Eval(_,_,e)   => isWhnf(e)
      case LetRec(_,e)   => isWhnf(e)
      case Match(_,alts) => alts forall (a => isWhnf(a.e))
      case Note(_,e)     => isWhnf(e)
    }
    
    def fresh(env:Env):String = {
      val s = Name(currName + "$" + currId)
      if (env hasFun (new Var(s))) {
        currId += 1
        fresh(env) 
      } else s toString
    }
    
    def l4Panic(s:String) = {
      assert(false,"l4c panic: " + s)
      null
    }
}
