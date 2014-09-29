/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.ast
package exp

import scala.annotation.tailrec
import scala.text.Document.{empty,group,nest,text}

import bluejelly.bjc.common.Name
import bluejelly.bjc.common.PprUtils._

/**
 * Base trait for expressions.
 * @author ppedemon
 */
trait Exp extends AstElem;

case class TySigExp(val e:Exp, val ty:types.Type) extends Exp {
  def ppr = gnest(Exp.pprPar(e) :/: "::" :/: ty.ppr)
}

case class InfixExp(val e0:Exp, val op:Name, val e1:Exp) extends Exp {
  def ppr = gnest(
      group(Exp.pprOperand(e0) :/: Name.asOp(op).ppr)  :/: 
      Exp.pprOperand(e1))
}

case class LeftSectExp(val e:Exp, val op:Name) extends Exp {
  def ppr = group(par((Exp.pprOperand(e) :: Name.asOp(op).ppr)))
}

case class RightSectExp(val op:Name, val e:Exp) extends Exp {
  def ppr = group(par((Name.asOp(op).ppr :: Exp.pprOperand(e))))
}

case class NegExp(val e:Exp) extends Exp {
  def ppr = group("-" :: Exp.pprPar(e))
}

case class ParExp(val e:Exp) extends Exp {
  def ppr = group(par(e.ppr))
}

case class CaseExp(val e:Exp, val alts:List[Alt]) extends Exp {
  def ppr = gnest(group("case" :/: e.ppr :/: text("of")) :/: pprBlock(alts))
}

case class LetExp(val ds:List[Decl], e:Exp) extends Exp {
  def ppr = gnest(
      group("let" :/: gnest(pprBlock(ds))) :/: 
      group("in" :/: e.ppr))
}

case class DoExp(val s:List[Stmt]) extends Exp {
  def ppr = gnest("do" :/: pprBlock(s))
}

case class MDoExp(val s:List[Stmt]) extends Exp {
  def ppr = gnest("mdo" :/: pprBlock(s))
}

case class LambdaExp(val ps:List[pat.Pat], e:Exp) extends Exp {
  def ppr = gnest(group("""\""" :/: pprMany(ps) :/: text("->")) :/: e.ppr)
}

case class IfExp(val cond:Exp, val et:Exp, val ef:Exp) extends Exp {
  def ppr = gnest(cat(List(
      group("if" :/: cond.ppr),
      group("then" :/: et.ppr),
      group("else" :/: ef.ppr))))
}

case class AppExp(val e:Exp, val arg:Exp) extends Exp {
  lazy val (head,args) = Exp.unwind(this)
  
  def isTuple = head match {
    case ConExp(TupleCon(_)) => true
    case _ => false
  }
  
  def ppr =
    if (isTuple) pprTuple(args) else {
      val as = args map Exp.pprPar
      head match {
        case ConExp(Con(n)) => group(cat(Name.asId(n).ppr :: as))
        case VarExp(n) => group(cat(Name.asId(n).ppr :: as))
        case _ => group(cat(head.ppr :: as))
      }
    }
}

case class VarExp(val v:Name) extends Exp {
  def ppr = v.ppr
}

case class ConExp(val con:GCon) extends Exp {
  def ppr = con.ppr
}

case class RecConExp(val con:Name, val binds:List[FBind]) extends Exp {
  def ppr = gnest(con.ppr :/: between("{",pprMany(binds,","),"}"))
}

case class RecUpdExp(val e:Exp, val binds:List[FBind]) extends Exp {
  def ppr = gnest(e.ppr :/: between("{",pprMany(binds,","),"}"))
}

case class ListExp(val es:List[Exp]) extends Exp {
  def ppr = between("[",pprMany(es,","),"]")
}

case class ListComp(val e:Exp, val qs:List[Stmt]) extends Exp {
  def ppr = between("[", gnest(
    group(e.ppr :/: text("|")) :/: 
    gnest(pprMany(qs,","))),"]")
}

case class EnumFromExp(val from:Exp) extends Exp {
  def ppr = between("[",group(from.ppr :: text("..")),"]")
}

case class EnumFromToExp(val from:Exp, val to:Exp) extends Exp {
  def ppr = between("[",group(from.ppr :: ".." :: to.ppr),"]")
}

case class EnumFromThenExp(val from:Exp, val `then`:Exp) extends Exp {
  def ppr = between("[",group(from.ppr :: "," :: `then`.ppr :: text("..")),"]")
}

case class EnumFromThenToExp(
    val from:Exp,
    val `then`:Exp,
    val to:Exp) extends Exp {
  def ppr = 
    between("[",group(from.ppr :: "," :: `then`.ppr :: ".." :: to.ppr),"]")
}

case class LitExp(val x:Lit) extends Exp {
  def ppr = x.ppr
}

object Exp {
  @tailrec
  def unwind(e:Exp, args:List[Exp] = Nil):(Exp,List[Exp]) = e match {
    case AppExp(e,arg) => unwind(e,arg::args)
    case _ => (e,args)
  }
  
  def appExp(es:List[Exp]) = es.reduceLeft((app,e) => AppExp(app,e))
  def tupleExp(es:List[Exp]) = appExp(ConExp(TupleCon(es.length))::es) 
  
  private[exp] def needsPar(e:Exp) = e match {
    case ParExp(_)|LeftSectExp(_,_)|RightSectExp(_,_) => false
    case ListExp(_)|EnumFromExp(_)|EnumFromToExp(_,_) => false
    case EnumFromThenExp(_,_)|EnumFromThenToExp(_,_,_) => false
    case VarExp(_)|ConExp(_)|LitExp(_) => false
    case a@AppExp(_,_) if a.isTuple => false
    case _ => true
  }
  
  private[exp] def pprPar(e:Exp) = 
    if (needsPar(e)) group(par(e.ppr)) else e.ppr
      
  private[exp] def pprOperand(e:Exp) = e match {
    case TySigExp(_,_) => pprPar(e)
    case _ => e.ppr
  }
}

// -----------------------------------------------------------------------
// Auxiliary definitions: guards, case alternatives, statements, rec binds
// -----------------------------------------------------------------------

class Guarded(val guard:Exp,val e:Exp) {
  def ppr(s:String) = gnest(group("|" :/: guard.ppr) :/: s :/: e.ppr)
}

trait AltRhs extends AstElem;
case class ExpAltRhs(val e:Exp) extends AltRhs { 
  def ppr = e.ppr
}
case class GrdAltRhs(val gs:List[Guarded]) extends AltRhs {
  def ppr = group(cat(gs map (_.ppr("->"))))
}
class Alt(val p:pat.Pat, val rhs:AltRhs, val ds:List[Decl]) extends AstElem {
  def ppr = {
    val w = if (ds.isEmpty) empty else gnest("where" :/: pprBlock(ds))
    val d = rhs match {
      case ExpAltRhs(_) => group(group(p.ppr :/: text("->")) :/: rhs.ppr)
      case GrdAltRhs(_) => group(p.ppr :/: rhs.ppr)
    }
    gnest(cat(d, w))
  }
}

trait Stmt extends AstElem;
case class FromStmt(val p:pat.Pat, val e:Exp) extends Stmt {
  def ppr = gnest(group(p.ppr :/: text("<-")) :/: e.ppr)
}
case class LetStmt(val ds:List[Decl]) extends Stmt {
  def ppr = if (ds.isEmpty) empty else gnest("let" :/: pprBlock(ds))
}
case class ExpStmt(val e:Exp) extends Stmt {
  def ppr = e.ppr
}

case class FBind(val v:Name, val e:Exp) extends AstElem {
  def ppr = group(v.ppr :/: "=" :/: e.ppr)
}
