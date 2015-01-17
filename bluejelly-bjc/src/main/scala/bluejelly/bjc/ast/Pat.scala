/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.ast
package pat

import scala.annotation.tailrec

import bluejelly.bjc.common.Name
import bluejelly.bjc.common.PprUtils._

import bluejelly.utils.Document.{group,text}

/**
 * Abstract trait for patterns.
 * @author ppedemon
 */
trait Pat extends AstElem;

case class ListPat(val ps:List[Pat]) extends Pat {
  def ppr = between("[",pprMany(ps, ","),"]")
}

case class RecPat(val con:Name, val bs:List[PBind]) extends Pat {
  def ppr = gnest(con.ppr :/: between("{", pprMany(bs,","), "}"))
}

case class InfixPat(val p0:Pat, val op:Name, val p1:Pat) extends Pat {
  def ppr = {
    gnest(p0.ppr :/: op.pprAsOp :/: p1.ppr)
  }
}

case class AppPat(val p:Pat, val arg:Pat) extends Pat {
  lazy val (head,allArgs) = Pat.unwind(this)
  
  def isTuple = head match {
    case ConPat(TupleCon(_)) => true
    case _ => false
  }
  
  def ppr = 
    if (isTuple) pprTuple(allArgs) else {
      val as = allArgs map Pat.pprPar
      head match {
        case ConPat(Con(n)) => group(cat(n.pprAsId :: as))
        case VarPat(n) => group(cat(n.pprAsId :: as))
        case _ => group(cat(head.ppr :: as))
      }
    }
}

case class NegPat(val p:Pat) extends Pat {
  def ppr = group("-" :: Pat.pprPar(p))
}

case class AsPat(val v:Name,val p:Pat) extends Pat {
  def ppr = group(v.ppr :: "@" :: Pat.pprPar(p))
}

case class LazyPat(val p:Pat) extends Pat {
  def ppr = group("~" :: Pat.pprPar(p))
}

case class ParPat(val p:Pat) extends Pat {
  def ppr = par(p.ppr)
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
  
  def appPat(con:Pat,ps:List[Pat]) = ps.foldLeft(con)(AppPat(_,_))
  def tuplePat(ps:List[Pat]) = appPat(ConPat(TupleCon(ps.length)), ps)
  
  private[pat] def pprPar(p:Pat) = 
    if (needsPar(p)) group(par(p.ppr)) else p.ppr
  
  private[pat] def needsPar(p:Pat) = p match {
    case ParPat(_)|ListPat(_)|ConPat(_)|VarPat(_)|LitPat(_)|WildPat => false
    case AsPat(_,_)|LazyPat(_) => false
    case a@AppPat(_,_) if a.isTuple => false
    case _ => true
  }
}

/*
 * Record pattern bindings
 */
case class PBind(val v:Name, val p:Pat) extends AstElem {
  def ppr = group(v.ppr :/: "=" :/: p.ppr)
}
