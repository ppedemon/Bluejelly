/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.ast
package types

import scala.text.Document.{group,text}
import bluejelly.bjc.common.Name
import bluejelly.bjc.common.PprUtils._
import bluejelly.bjc.common.PrettyPrintable
import scala.annotation.tailrec

/*
 * Abstract syntax for types.
 */
trait Type extends AstElem

case class PolyType(val tyvars:List[Name],ty:Type) extends Type {
  def ppr = gnest("forall" :/: pprMany(tyvars) :/: "." :/: ty.ppr)
}

case class QualType(val ctx:List[Pred], ty:Type) extends Type {
  def ppr = gnest(pprTuple(ctx) :/: "=>" :/: ty.ppr)
}

case class FunType(val from:Type, val to:Type) extends Type {
  def ppr = from match {
    case FunType(_,_) => gnest(between("(",from.ppr,")") :/: "->" :/: to.ppr)
    case _ => gnest(from.ppr :/: "->" :/: to.ppr) 
  } 
}

case class AppType(val fun:Type, val arg:Type) extends Type {
  def ppr = fun match {
    case ListCon => group("[" :: arg.ppr :: text("]"))
    case _ => arg match {
      case AppType(_,_)|FunType(_,_) => 
        gnest(fun.ppr :/: between("(",arg.ppr,")"))
      case _ => 
        gnest(fun.ppr :/: arg.ppr) 
    } 
  }
}

case object ArrowCon extends Type { def ppr = text("(->)") }
case object UnitCon  extends Type { def ppr = text("()") }
case object ListCon  extends Type { def ppr = text("[]") }

case class TupleCon(val arity:Int) extends Type { 
  def ppr = text("(%s)" format (","*arity))
}

case class TyCon(val name:Name) extends Type { def ppr = name.ppr }
case class TyVar(val name:Name) extends Type { def ppr = name.ppr }

case object AnonTyVar extends Type {
  import bluejelly.bjc.common.NameSupply.freshTyVar
  def name = freshTyVar
  def ppr = text("#t%s" format name)
}

/*
 * Predicates: head representing constraint, plus arguments
 */
class Pred(val head:Name, tys:List[Type]) extends PrettyPrintable {
  def ppr = group(head.ppr :/: pprMany(tys))
}

/**
 * Convenience functions operating over {{Type}}s.
 * @author ppedemon
 */
object Type {
  @tailrec
  def unwind(ty:Type, args:List[Type] = Nil):(Type,List[Type]) = ty match {
    case AppType(fun,arg) => unwind(fun,arg::args)
    case _ => (ty,args.reverse)
  }
  
  def mkPred = new PartialFunction[Type,Pred] {
    def isDefinedAt(ty:Type) = unwind(ty) match {
      case (TyCon(_),_) => true
      case _ => false
    }
    def apply(ty:Type) = unwind(ty) match {
      case (TyCon(name),args) => new Pred(name,args)
    }
  }
}
