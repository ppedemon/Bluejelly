/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.l4.test

import bluejelly.l4.Var
import bluejelly.l4.Expr
import bluejelly.l4.ELit
import bluejelly.l4.ECon
import bluejelly.l4.App
import bluejelly.l4.NApp
import bluejelly.l4.Let
import bluejelly.l4.LetRec
import bluejelly.l4.Eval
import bluejelly.l4.Match
import bluejelly.l4.Pat
import bluejelly.l4.PLit
import bluejelly.l4.PVar
import bluejelly.l4.PCon
import bluejelly.l4.Alt
import bluejelly.l4.Decl
import bluejelly.l4.FunDecl
import bluejelly.l4.Module
import bluejelly.l4.ELit
import bluejelly.l4.ECon
import bluejelly.l4.Note

/**
 * General utilities for AST testing.
 * @author ppedemon
 */
class AstUtils {

  // ---------------------------------------------------------------------
  // Check for module, function and expression isomorphisms
  // ---------------------------------------------------------------------
  
  type E = Map[Var,Var]
  type M = Map[Var,FunDecl]

  /**
   * Are the given modules isomorphic? They will, if both modules
   * have the same number of functions, and if for every function
   * f in m0, there is a function g in m1 where f and g are
   * isomorphic.
   */
  def isoMod(m0:Module,m1:Module) = isoDecls(m0.decls, m1.decls)
  
  private def isoDecls(ds0:List[Decl],ds1:List[Decl]) = {
    val m0 = collectFuns(ds0)
    val m1 = collectFuns(ds1)
    m0.size == m1.size && m0.forall {
      case (n,f) =>
        if (m1 isDefinedAt n) {
          val g = m1(n)
          isoFun(f,g)
        } else false
    }
  } 
   
  private def collectFuns(ds:List[Decl]):M = ds match {
    case Nil => Map()
    case (f@FunDecl(n,_,_))::ds => collectFuns(ds) + (n->f)
    case _::ds => collectFuns(ds)
  }
  
  /** 
   * Two functions are isomorphic if they have the same number of
   * arguments and if their bodies are isomorphic.
   */
  def isoFun(f:FunDecl, g:FunDecl) = {
    if (f.args.length != g.args.length) false else {
      val e = (f.args,g.args).zipped.foldLeft(Map():E){case (m,(u,v)) => m + (u->v)}
      iso(e)(f.body,g.body)
    }
  }
  
  /**
   * Check if the given two expressions are isomorphic. Our definition
   * isomorphic is: expressions are alpha-convertible.
   * 
   * However, we are senitive to qualifiers: f M.x and f x are, with M 
   * the name of the enclosing module, are *not* isomorphic. So, you must 
   * be sure that you are passing two expressions where qualifiers won't
   * force false negatives.
   * 
   * <p> Since we will use this for testing only, we can always be sure 
   * that we are going to compare expressions where name qualifiers are 
   * not an issue. We, in the role of testers using  this class, will 
   * take this as a precondition.
   */
  def iso(e:E)(x:Expr,y:Expr):Boolean = (x,y) match {
    case (ELit(x),ELit(y)) => x == y
    case (ECon(c,xs),ECon(d,ys)) => c == d && isoList(e)(xs,ys)

    case (App(f,xs),App(g,ys)) => 
      val (extEnv,ok) = isoVar(e)(f,g)
      if (ok) isoList(extEnv)(xs,ys) else false
    case (NApp(f,xs),NApp(g,ys)) =>
      val (extEnv,ok) = isoVar(e)(f,g)
      if (ok) isoList(extEnv)(xs,ys) else false

    case (Let(x,exp0,b0),Let(y,exp1,b1)) =>
      val ok = iso(e)(exp0,exp1)
      ok && iso(e + (x->y))(b0,b1)
    case (Eval(x,exp0,b0),Eval(y,exp1,b1)) =>
      val ok = iso(e)(exp0,exp1)
      ok && iso(e + (x->y))(b0,b1)

    case (LetRec(ds0,b0),LetRec(ds1,b1)) => 
      val (extEnv,ok) = isoDecls(e)(ds0,ds1)
      ok && iso(extEnv)(b0,b1)

    case (Match(u,as0),Match(v,as1)) =>
      val (extEnv,ok) = isoVar(e)(u,v)
      (as0,as1).zipped.forall(isoAlt(extEnv))
    
    // Ignore occurrence info
    case (Note(_,x),y) => iso(e)(x,y)
    case (x,Note(_,y)) => iso(e)(x,y)
    case _ => false
  }

  def isoList(e:E)(xs:List[Expr], ys:List[Expr]) = 
    xs.length == ys.length && (xs,ys).zipped.forall(iso(e))

  private def isoVar(e:E)(u:Var,v:Var) = 
    if (e isDefinedAt u) (e,e(u) == v) else (e + (u->v),true) 
    
  private def isoDecls(e:E)(xs:List[(Var,Expr)], ys:List[(Var,Expr)]) = {
    val (ds0,es0) = xs unzip
    val (ds1,es1) = ys unzip
    val extEnv = (ds0,ds1).zipped.foldLeft(e){case (e,(u,v)) => e + (u->v)}
    (extEnv, es0.length == es1.length && isoList(extEnv)(es0,es1))
  }
  
  private def isoPat(e:E)(p:Pat,q:Pat) = (p,q) match {
    case (PLit(l0),PLit(l1)) => (e,l0 == l1)
    case (PVar(u),PVar(v)) => (e + (u->v),true)
    case (PCon(c,xs),PCon(d,ys)) => 
      val extEnv = (xs,ys).zipped.foldLeft(e){case (e,(u,v)) => e + (u->v)}
      (extEnv, c == d)
    case _ => (e,false)
  }
  
  private def isoAlt(e:E)(a0:Alt,a1:Alt) = {
    val (extEnv,ok) = isoPat(e)(a0.p,a1.p)
    ok && iso(extEnv)(a0.e,a1.e)    
  }

  
  // ---------------------------------------------------------------------
  // Check for module, function and expression exact equality
  // ---------------------------------------------------------------------

  def eqMod(m0:Module,m1:Module)= 
    m0.n == m1.n && eqDecls(m0.decls, m1.decls)
    
  private def eqDecls(ds0:List[Decl],ds1:List[Decl]) = {
    val m0 = collectFuns(ds0)
    val m1 = collectFuns(ds1)
    m0.size == m1.size && m0.forall {
      case (n,f) =>
        if (m1 isDefinedAt n) {
          val g = m1(n)
          eqFun(f,g)
        } else false
    }    
  }
  
  def eqFun(f:FunDecl,g:FunDecl) = 
    f.args == g.args && f.body == g.body
}
