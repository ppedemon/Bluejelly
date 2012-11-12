/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.l4

import bluejelly.utils.Name
import scala.util.parsing.input.Positional
import scala.util.parsing.input.Position

/**
 * Big bag of AST classes for l4 (low level lazy language).
 * @author ppedemon
 */

trait Positionable extends Positional {
  def at[T <: Positional](p:T):this.type = { 
    this.pos = p.pos
    this
  }
}

class ConDef(val tag:Int, val arity:Int)

sealed abstract class Id(val n:Name) {
  override def toString = n toString
  override def hashCode = n hashCode
}

class Var(n:Name) extends Id(n) {
  override def equals(v:Any) = v match { 
    case v:Var => v.n equals n
    case _ => false
  }
}

class ConRef(n:Name) extends Id(n) {
  override def equals(c:Any) = c match { 
    case c:ConRef => c.n equals n
    case _ => false
  }
}

sealed abstract class Occ
case object Never extends Occ
case object Once extends Occ
case object Many extends Occ

sealed abstract class Lit
case class IntLit(val i:Int) extends Lit
case class DblLit(val d:Double) extends Lit
case class ChrLit(val c:Char) extends Lit
case class StrLit(val s:String) extends Lit
object Lit {
  def value(n:Lit):Any = n match {
    case IntLit(x) => x
    case DblLit(x) => x
    case ChrLit(x) => x
    case StrLit(x) => x
  }
}

sealed abstract class Expr extends Positionable
case class ELit(val lit:Lit) extends Expr
case class ECon(val c:ConRef, val args:List[Expr]) extends Expr
case class App(val fun:Var, val args:List[Expr]) extends Expr
case class NApp(val fun:Var, var args:List[Expr]) extends Expr
case class Let(val v:Var, val exp:Expr, val body:Expr) extends Expr
case class LetRec(val decls:List[(Var,Expr)], body: Expr) extends Expr
case class Eval(val v:Var, val exp:Expr, val body:Expr) extends Expr
case class Match(var v:Var, val alts:List[Alt]) extends Expr
case class Note(val occ:Occ, val expr:Expr) extends Expr

class Alt(val p:Pat, val e:Expr) {
  def isVarAlt = p match {
    case PVar(_) => true
    case _ => false
  }  
}

sealed abstract class Pat(val vars:List[Var]) extends Positionable
case class PVar(val v:Var) extends Pat(List(v))
case class PCon(val c:ConRef, override val vars:List[Var]) extends Pat(vars)
case class PLit(val lit:Lit) extends Pat(List())
object Pat {
  def sameType(p:Pat, q:Pat):Boolean = (p,q) match {
    case (PVar(_),_) => true
    case (_,PVar(_)) => true
    case (PCon(_,_),PCon(_,_)) => true
    case (PLit(lp),PLit(lq)) => lp.getClass == lq.getClass
    case _ => false
  }
}

sealed trait Decl extends Positionable
case class DataDecl(val ref:ConRef, val con:ConDef) extends Decl
case class FunDecl(val n:Var, val args:List[Var], val body:Expr) extends Decl

class Module(val n:Name, val decls:List[Decl])

// -----------------------------------------------------------------------
// Pretty printing
// -----------------------------------------------------------------------

object PrettyPrinter {
  import scala.text._
  import scala.text.Document._
  
  def cat(d0:Document, d1:Document):Document = d0 match {
    case DocNil => d1
    case _ => d1 match {
      case DocNil => d0
      case _ => d0 :/: d1
    }
  }
  
  def ppr(v:Var):Document = text(v.n.toString)
  def ppr(c:ConRef):Document = text(c.n.toString)
  def ppr(con:ConDef):Document = text("{%d,%d}" format (con.tag, con.arity))
  
  def pprVars(vars:List[Var]):Document = 
    (vars :\ (empty:Document))((v,d) => cat(ppr(v),d))
  
  def ppr(occ:Occ):Document = occ match {
    case Never => text("[0]")
    case Once  => text("[1]")
    case Many  => text("[*]")
  }
  
  def ppr(lit:Lit):Document = lit match {
    case IntLit(i) => text(i.toString)
    case DblLit(d) => text(d.toString)
    case ChrLit(c) => text("'%s'" format c.toString)
    case StrLit(s) => text(""""%s"""" format s)
  }

  def ppr(p:Pat):Document = p match {
    case PVar(v)      => ppr(v)
    case PLit(lit)    => ppr(lit)
    case PCon(c,args) => group(cat(ppr(c),pprVars(args)))
  }

  def pprArg(arg:Expr):Document = arg match {
    case ELit(lit) => ppr(lit)
    case App(e,List()) => ppr(e)
    case ECon(c,List()) => ppr(c)
    case _ => group ("(" :: ppr(arg) :: text(")"))
  }

  def pprArgs(args:List[Expr]):Document = 
    (args :\ (empty:Document)) ((v,d) => cat(pprArg(v),d))
  
  def pprApp(app:App):Document   = group(cat(ppr(app.fun), pprArgs(app.args)))
  def pprNApp(app:NApp):Document = group("@" :: cat(ppr(app.fun), pprArgs(app.args)))
  def pprCon(con:ECon):Document  = group(cat(ppr(con.c), pprArgs(con.args))) 
  
  def pprDecl:((Var,Expr)) => Document = {case (v,e) => 
    nest(2,group(ppr(v) :: " =" :/: ppr(e)))
  }

  def pprDecls(ds:List[(Var,Expr)]):Document = ds match {
    case List()  => empty
    case List(d) => pprDecl(d)
    case d :: ds => pprDecl(d) :/: "and" :/: pprDecls(ds)
  }

  def pprAlt(alt:Alt):Document = 
    group(nest(2, group(ppr(alt.p) :/: text("->")) :/: ppr(alt.e)))
  
  def pprAlts(alts:List[Alt]):Document = 
    group((alts :\ (empty:Document)) ((v,d) => "| " :: cat(pprAlt(v),d)))
  
  def ppr(e:Expr):Document = e match {
    case ELit(lit)     => ppr(lit)
    case e@ECon(_,_)   => pprCon(e)
    case e@App(_,_)    => pprApp(e)
    case e@NApp(_,_)   => pprNApp(e)
    case Let(v,e,b)    => 
      group(nest(2, group("let" :/: pprDecl(v,e) :/: text("in")) :/: ppr(b)))
    case Eval(v,e,b)   => 
      group(nest(2, group("let!":/: pprDecl(v,e) :/: text("in")) :/: ppr(b)))
    case Note(occ,e)   => 
      group(nest(2, ppr(occ) :: group("(" :: ppr(e) :: text(")"))))
    case Match(v,alts) => 
      group(nest(2, group("match" :/: ppr(v) :/: text("with")) :/: pprAlts(alts)))
    case LetRec(ds,b)  => 
      group(nest(2, group("let rec" :/: pprDecls(ds) :/: text("in")) :/: ppr(b)))
  }
  
  def ppr(d:Decl):Document = d match {
    case DataDecl(cref,cdef) => 
      group("data" :/: ppr(cref) :: ppr(cdef))
    case FunDecl(n,args,e) =>
      group(nest(2,group("fun" :/: ppr(n) :/: cat(pprVars(args),text("="))) :/: ppr(e)))
  }
  
  def nl:Document = text("\n")
  
  def pprTopDecls(decls:List[Decl]):Document = 
    (decls :\ (empty:Document))((v,d) => cat(ppr(v), nl :: d))
  
  def ppr(m:Module):Document = 
    "module " :: text(m.n.toString) :: nl :: group(pprTopDecls(m.decls))
}
