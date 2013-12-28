/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.ast
package types

import scala.text.Document.{group,text}
import scala.annotation.tailrec

import bluejelly.bjc.common.Name
import bluejelly.bjc.common.PprUtils._
import bluejelly.bjc.common.PrettyPrintable

/*
 * Abstract syntax for types.
 */
trait Type extends AstElem

case class PolyType(val tyvars:List[Name],ty:Type) extends Type {
  def ppr = gnest("forall" :/: pprMany(tyvars) :/: text(".") :/: ty.ppr)
}

case class QualType(val ctx:List[Pred], ty:Type) extends Type {
  def ppr = gnest(
    (if (ctx.length == 1) ctx.head.ppr else pprTuple(ctx)) :/: "=>" :/: ty.ppr)
}

case class AppType(val fun:Type, val arg:Type) extends Type {
  lazy val (head,allArgs) = Type.unwind(this)

  def isTuple = head match {
    case TyCon(TupleCon(_)) => true
    case _ => false
  }
  def isFun = head match {
    case TyCon(ArrowCon) => true
    case _ => false
  }
  def isList = fun match {
    case TyCon(ListCon) => true
    case _ => false
  }
  
  def ppr = 
    if (isTuple) pprTuple(allArgs) else
    if (isList) group(between("[",arg.ppr,"]")) else
    if (isFun) {
      val (left,right) = allArgs match {
        case List(f@AppType(_,_),x) if f.isFun => (par(f.ppr),x.ppr)
        case List(f@PolyType(_,_),x) => (par(f.ppr),x.ppr)
        case List(f,x) => (f.ppr,x.ppr)
        case _ => sys.error("Illegal function args: %s" format allArgs)
      }
      group(left :/: "->" :/: right)
    } else
    arg match {
      case a@AppType(_,_) if !a.isTuple => 
        group(fun.ppr :/: par(arg.ppr))
      case PolyType(_,_) =>
        group(fun.ppr :/: par(arg.ppr))
      case _ => 
        group(fun.ppr :/: arg.ppr) 
    } 
}

case class TyVar(val name:Name) extends Type { def ppr = name.ppr }
case class TyCon(val tycon:GCon) extends Type { def ppr = tycon.ppr }

case class AnonTyVar() extends Type {
  import bluejelly.bjc.common.NameSupply.freshTyVar
  val name = freshTyVar
  def ppr = text("t%s" format name)
}

/*
 * Predicates: head representing constraint, plus arguments
 */
class Pred(val head:Name, val tys:List[Type]) extends PrettyPrintable {
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
    case _ => (ty,args)
  }
  
  def tyCon(n:Name) = TyCon(Con(n))
  def listCon = TyCon(ListCon)
  def unitCon = TyCon(UnitCon)
  def arrowCon = TyCon(ArrowCon)
  def tupleCon(arity:Int) = TyCon(TupleCon(arity))
  
  def mkApp(fun:Type, args:List[Type]) = args.foldLeft(fun)(AppType(_,_))
  
  def mkFun(tys:List[Type]) = 
    tys.reduceRight((x,y) => AppType(AppType(TyCon(ArrowCon),x),y))
  def mkFun(tys:List[Type],ty:Type) = 
    tys.foldRight(ty)((x,y) => AppType(AppType(TyCon(ArrowCon),x),y))
  
  def mkPred = new PartialFunction[Type,Pred] {
    def isDefinedAt(ty:Type) = unwind(ty) match {
      case (TyCon(_),_) => true
      case _ => false
    }
    def apply(ty:Type) = unwind(ty) match {
      case (TyCon(Con(name)),args) => new Pred(name,args)
    }
  }  
}
