/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.ast

import scala.annotation.tailrec
import scala.text.Document.{group,text}

import bluejelly.bjc.common.Name
import bluejelly.bjc.common.PprUtils.{between,par,gnest,pprMany,pprTuple}

/**
 * Abstract trait for patterns.
 * @author ppedemon
 */
trait Pat extends AstElem;

case class TySigPat(val p:Pat, val ty:types.Type) extends Pat {
  def ppr = p.ppr :/: "::" :/: ty.ppr
}

case class ListPat(val ps:List[Pat]) extends Pat {
  def ppr = between("[",pprMany(ps, ","),"]")
}

case class RecPat(val con:Name, val bs:List[PBind]) extends Pat {
  def ppr = gnest(con.ppr :/: between("{", pprMany(bs,","), "}"))
}

case class InfixPat(val p0:Pat, val op:Name, val p1:Pat) extends Pat {
  def ppr = {
    val d0 = p0 match {
      case InfixPat(_,_,_) => par(p0.ppr)
      case _ => p0.ppr
    }
    val d1 = p1 match {
      case InfixPat(_,_,_) => par(p1.ppr)
      case _ => p1.ppr
    }
    gnest(d0 :/: op.ppr :/: d1) 
  }
}

case class AppPat(val p:Pat, val arg:Pat) extends Pat {
  lazy val (head,allArgs) = Pat.unwind(this)
  
  def isTuple = head match {
    case ConPat(TupleCon(_)) => true
    case _ => false
  }
  def isList = head match {
    case ConPat(ListCon) => true
    case _ => false
  }
  
  def ppr = 
    if (isTuple) pprTuple(allArgs) else
    if (isList) group(between("[",arg.ppr,"]")) else
    arg match {
      case a@AppPat(_,_) if !a.isTuple => group(p.ppr :/: par(arg.ppr))
      case InfixPat(_,_,_) => group(p.ppr :/: par(arg.ppr))
      case _ => group(p.ppr :/: arg.ppr) 
    } 
}

case class NegPat(val p:Pat) extends Pat {
  def ppr = group("-" :: (if (Pat.needsPar(p)) par(p.ppr) else p.ppr))
}

case class AsPat(val v:Name,val p:Pat) extends Pat {
  def ppr = group(v.ppr :: "@" :: (if (Pat.needsPar(p)) par(p.ppr) else p.ppr))
}

case class LazyPat(val p:Pat) extends Pat {
  def ppr = group("~" :: (if (Pat.needsPar(p)) par(p.ppr) else p.ppr))
}

case class ConPat(val con:GCon) extends Pat {
  def ppr = con.ppr
}

case class VarPat(val n:Name) extends Pat {
  def ppr = n.ppr
}

case class LitPat(val x:Lit) extends Pat {
  def ppr = x.ppr
}

case object WildPat extends Pat {
  def ppr = text("_")
}

/*
 * Pattern utility functions
 */
object Pat {
  @tailrec
  def unwind(p:Pat, args:List[Pat] = Nil):(Pat,List[Pat]) = p match {
    case AppPat(p,arg) => unwind(p,arg::args)
    case _ => (p,args)
  }
  
  def needsPar(p:Pat) = p match {
    case ListPat(_)|ConPat(_)|VarPat(_)|LitPat(_)|WildPat => false
    case a@AppPat(_,_) if a.isTuple || a.isList => false
    case _ => true
  }
}

/*
 * Record pattern bindings
 */
trait PBind extends AstElem;
case class VarPBind(val v:Name) extends PBind { def ppr = v.ppr }
case class AsgPBind(val v:Name, val p:Pat) extends PBind {
  def ppr = group(v.ppr :/: "=" :/: p.ppr)
}
