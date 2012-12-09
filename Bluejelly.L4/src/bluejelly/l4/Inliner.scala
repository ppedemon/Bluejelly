/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.l4

import scala.collection.immutable.Nil
import scala.util.control.Exception._

/**
 * Poor man's inliner. Dead simple.
 * @author ppedemon
 */
object Inliner {
  private type Env = Map[Var,Expr]
  
  private def inlineExpr(expr:Expr,env:Env):Expr = expr match {
    case ELit(_) => expr
    case ECon(c,args) => ECon(c, args map (e => inlineExpr(e,env)))
    case App(v,args)  => inlineApp(v, args, env, true)
    case NApp(v,args) => inlineApp(v, args, env, false)
    
    case Let(v,Note(Never,e),b) => inlineExpr(b,env)
    case Let(v,Note(Once,e),b)  => inlineExpr(b, env + (v -> inlineExpr(e,env)))
    case Let(v,e,b) if trivialVar(e,env) => inlineExpr(b, env + (v -> inlineExpr(deAnn(e),env)))
    case Let(v,e,b) if trivial(e) => inlineExpr(b, env + (v -> inlineExpr(deAnn(e),env)))
    case Let(v,e,b) => Let(v, inlineExpr(e,env), inlineExpr(b, env - v))
    
    case Eval(v,e,b) if trivial(e) => 
      inlineExpr(b, env + (v -> inlineExpr(deAnn(e),env)))
    case Eval(v,Note(Once,e),b) => {
      val ie = inlineExpr(e,env)
      if (firstUse(v,b)) {
        inlineExpr(b, env + (v -> Eval(v,ie,App(v,Nil))))
      } else {
        val ib = inlineExpr(b,env)
        if (firstUse(v,ib)) {
          inlineExpr(ib, Map(v -> Eval(v,ie,App(v,Nil))))
        } else {
          Eval(v,Note(Once,ie),ib)
        }
      }
    }
    case Eval(v,e,b) => Eval(v, inlineExpr(e,env), inlineExpr(b, env - v))
    
    case LetRec(decls,e) => {
      val (idecls,ienv) = inlineDecls(env,decls)
      LetRec(idecls, inlineExpr(e, ienv))
    } 
    
    case Match(v,alts) if env isDefinedAt v => 
      Eval(v, Note(Once,env(v)), Match(v, alts map inlineAlt(env)))
    case Match(v,alts) => Match(v, alts map inlineAlt(env))
    
    case Note(n,e) => Note(n, inlineExpr(e,env))
  }
  
  private def inlineDecls(env:Env, decls:List[(Var,Expr)]) = {
    val ienv = env -- (decls map (_._1))
    val idecls = for ((id,e) <- decls) yield (id, inlineExpr(e,ienv))
    (idecls,ienv)
  }
  
  private def inlineAlt(env:Env)(alt:Alt) = 
    new Alt(alt.p, inlineExpr(alt.e, env -- alt.p.vars))
  
  private def inlineApp(v:Var, args:List[Expr], env:Env, isUpd:Boolean) = args match {
    case Nil => env getOrElse (v, mkApp(v,Nil,isUpd))
    case _   => {
      val iargs = args map (e => inlineExpr(e,env))
      if (env isDefinedAt v) env(v) match {
          case App(v,args)  => mkApp(v, args ++ iargs, isUpd)
          case NApp(v,args) => mkApp(v, args ++ iargs, isUpd)
          case Eval(x,e,App(y,Nil)) if x == y => Eval(x,e,mkApp(x,iargs,isUpd))
          case e => Let(v,e,mkApp(v,iargs,isUpd))
        }
      else
        mkApp(v,iargs,isUpd)
    }
  }
  
  private def mkApp(v:Var, args:List[Expr], isUpd:Boolean) = 
    if (isUpd) App(v, args) else NApp(v, args)
  
  private def trivialVar(e:Expr, env:Env):Boolean = e match {
    case App(v,Nil) => 
      (handling(classOf[NoSuchElementException]) by (_ => false))(trivial(env(v)))
    case Note(_,e) => trivialVar(e,env)
    case _ => false
  }
    
  private def trivial(e:Expr):Boolean = e match {
    case ELit(_) | ECon(_,Nil) | NApp(_,Nil) => true
    case Note(_,e) => trivial(e)
    case _ => false
  }
  
  private def deAnn(e:Expr):Expr = e match {
    case Note(_,e) => deAnn(e)
    case _ => e
  }
  
  private def firstUse(v:Var, e:Expr) = first(v)(false,e)
  
  private def first(v:Var)(f:Boolean, e:Expr):Boolean = e match {
    case ELit(_) => f
    case ECon(_,args)  => (f /: args)(first(v))
    case App(id,args)  => firstApp(v, f, id, args)
    case NApp(id,args) => firstApp(v, f, id, args)
    case Let(_,e,b)    => (f /: List(e,b))(first(v))
    case LetRec(decls,b) => (f /: ((decls map (_._2)) ::: List(b)))(first(v))
    case Eval(v,e,_) => first(v)(false, e)
    case Match(v,alts) => false
    case Note(_,e) => first(v)(f,e)
  }
  
  private def firstApp(v:Var, f:Boolean, id:Var, args:List[Expr]) = args match {
    case Nil if id == v => true
    case Nil => f
    case _ => (f /: (App(id,Nil)::args))(first(v))
  }
    
  private def inlineDecl(d:Decl) = d match {
    case f@FunDecl(n, args, b) => FunDecl(n, args, inlineExpr(b, Map())) at f
    case _ => d
  }
  
  def inline(m:Module) = new Module(m.n, m.decls map inlineDecl)
  
}
