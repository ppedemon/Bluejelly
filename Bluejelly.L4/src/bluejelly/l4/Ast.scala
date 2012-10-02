/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.l4

import bluejelly.utils.Name

/**
 * Big bag of AST classes for l4 (low level lazy language).
 * @author ppedemon
 */

sealed trait Id
class Var(val n:Name) extends Id
class ConRef(val n:Name) extends Id
class ConDef(val tag:Int, val arity:Int)

sealed trait Occ
case object Never extends Occ
case object Once extends Occ
case object Many extends Occ

sealed trait Lit
case class IntLit(val i:Int) extends Lit
case class DblLit(val d:Double) extends Lit
case class ChrLit(val c:Char) extends Lit
case class StrLit(val s:String) extends Lit

sealed trait Expr
case class ELit(val lit:Lit) extends Expr
case class ECon(val c:ConRef, val args:List[Expr]) extends Expr
case class App(val fun:Var, val args:List[Expr]) extends Expr
case class NApp(val fun:Var, var args:List[Expr]) extends Expr
case class Let(val v:Var, val exp:Expr, val body:Expr) extends Expr
case class LetRec(val decls:List[(Var,Expr)], body: Expr) extends Expr
case class Eval(val v:Var, val exp:Expr, val body:Expr) extends Expr
case class Match(var v:Var, val alts:List[Alt]) extends Expr
case class Note(val occ:Occ, val expr:Expr) extends Expr

class Alt(val p:Pat, val e:Expr)

sealed trait Pat
case class PVar(val v:Var) extends Pat
case class PCon(val c:ConRef, val vars:List[Var]) extends Pat
case class PLit(val lit:Lit) extends Pat

sealed trait Decl
case class DataDecl(val ref:ConRef, val con:ConDef) extends Decl
case class FunDecl(val n:Var, val args:List[Var], val body:Expr) extends Decl

class Module(val n:Name, val decls:List[Decl])

// -----------------------------------------------------------------------
// Pretty printing
// -----------------------------------------------------------------------

object PrettyPrinter {
  import scala.text._
  import scala.text.Document._
  
  def ppr(v:Var):Document = text(v.n.toString)
  def ppr(c:ConRef):Document = text(c.n.toString)
  def ppr(con:ConDef):Document = text("{%d,%d}" format (con.tag, con.arity))
  
  def pprVars(vars:List[Var]):Document = 
    group((vars :\ (empty:Document)) (ppr(_) :/: _))
  
  def ppr(occ:Occ):Document = occ match {
    case Never => text("[0]")
    case Once  => text("[1]")
    case Many  => text("[*]")
  }
  
  def ppr(lit:Lit):Document = lit match {
    case IntLit(i) => text(i.toString)
    case DblLit(d) => text(d.toString)
    case ChrLit(c) => text(c.toString)
    case StrLit(s) => text(s)
  }

  def ppr(p:Pat):Document = p match {
    case PVar(v)      => ppr(v)
    case PLit(lit)    => ppr(lit)
    case PCon(c,args) => group(ppr(c) :: pprVars(args))
  }

  def pprArg(arg:Expr):Document = arg match {
    case App(e,List()) => ppr(e)
    case _ => group ("(" :: ppr(arg) :: text(")"))
  }

  def pprArgs(args:List[Expr]):Document = 
    (args :\ (empty:Document)) (pprArg(_) :/: _)
  
  def pprApp(app:App):Document   = group(ppr(app.fun) :/: pprArgs(app.args))
  def pprNApp(app:NApp):Document = group("@" :: ppr(app.fun) :/: pprArgs(app.args))
  def pprCon(con:ECon):Document  = group(ppr(con.c) :/: pprArgs(con.args)) 
  
  def pprDecl:((Var,Expr)) => Document = {case (v,e) => 
    group(ppr(v) :: "=" :/: ppr(e))
  }

  def pprDecls(ds:List[(Var,Expr)]):Document = ds match {
    case List()  => empty
    case List(d) => pprDecl(d)
    case d :: ds => pprDecl(d) :/: "and" :/: pprDecls(ds)
  }

  def pprAlt(alt:Alt):Document = group(ppr(alt.p) :: "->" :/: ppr(alt.e))
  
  def pprAlts(alts:List[Alt]):Document = 
    group((alts :\ (empty:Document)) ("|" :: pprAlt(_) :/: _))
  
  def ppr(e:Expr):Document = e match {
    case ELit(lit)     => ppr(lit)
    case e@ECon(_,_)   => pprCon(e)
    case e@App(_,_)    => pprApp(e)
    case e@NApp(_,_)   => pprNApp(e)
    case Let(v,e,b)    => group ("let" :/: pprDecl(v,e) :/: "in" :/: ppr(b))
    case Eval(v,e,b)   => group ("let!" :/: pprDecl(v,e) :/: "in" :/: ppr(b))
    case Note(occ,e)   => group(ppr(occ) :/: group("(" :: ppr(e) :: text(")")))
    case Match(v,alts) => group("match" :: ppr(v) :: "with" :/: nest(2, pprAlts(alts)))
    case LetRec(ds,b)  => group ("let rec" :/: nest(2, group(pprDecls(ds))) :/: "in" :/: ppr(b))
  }
  
  def ppr(d:Decl):Document = d match {
    case DataDecl(cref,cdef) => 
      group("data" :/: ppr(cref) :: ppr(cdef))
    case FunDecl(n,args,e) =>
      group("fun" :/: ppr(n) :/: pprVars(args) :/: "=" :/: ppr(e))
  }
  
  def nl2:Document = text("\n\n")
  
  def pprTopDecls(decls:List[Decl]):Document = 
    group((decls :\ (empty:Document)) (ppr(_) :: nl2 :/: _))
  
  def ppr(m:Module):Document = 
    group("module" :: m.n.toString :: nl2 :/: pprTopDecls(m.decls))
}
