/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.iface

import scala.annotation.tailrec
import scala.text.Document.{text,group}

import bluejelly.bjc.common.PprUtils._
import bluejelly.bjc.common.PrettyPrintable
import bluejelly.bjc.common.{Name,Unqual,Qual}
import bluejelly.bjc.ast.{GCon,UnitCon,TupleCon,ArrowCon,ListCon,Con}

/**
 * Interface kinds.
 * @author ppedemon
 */
trait IfaceKind extends PrettyPrintable
case object IfaceKStar extends IfaceKind { def ppr = text("*") }
case class IfaceKFun(val from:IfaceKind, val to:IfaceKind) extends IfaceKind {
  def ppr = from match {
    case IfaceKFun(_,_) => gnest(par(from.ppr) :/: "->" :/: to.ppr)
    case _ => gnest(from.ppr:/: "->" :/: to.ppr)
  }
}

/**
 * An interface type variable.
 * @author ppedemon
 */
class IfaceTyVar(val name:Name, val kind:IfaceKind) extends PrettyPrintable {
  //def ppr = gnest(name.ppr :/: ":::" :/: kind.ppr)
  def ppr = name.ppr
}

/**
 * Type information for things available in an interface file.
 * @author ppedemon
 */
trait IfaceType extends PrettyPrintable

case class IfacePolyTy(
    val tvs:List[IfaceTyVar], val ty:IfaceType) extends IfaceType {
  def ppr = gnest("forall" :/: pprMany(tvs) :/: text(".") :/: ty.ppr)
} 

case class IfaceQualTy(
    val ctx:List[IfacePred], val ty:IfaceType) extends IfaceType {
  def ppr = gnest(
    (if (ctx.length == 1) ctx.head.ppr else pprTuple(ctx)) :/: "=>" :/: ty.ppr)
}

case class IfaceAppTy(val fun:IfaceType, val arg:IfaceType) extends IfaceType {
  lazy val (head,allArgs) = IfaceType.unwind(this)

  def isTuple = head match {
    case IfaceTcTy(TupleCon(_)) => true
    case _ => false
  }
  def isFun = head match {
    case IfaceTcTy(ArrowCon) => true
    case _ => false
  }
  def isList = fun match {
    case IfaceTcTy(ListCon) => true
    case _ => false
  }
  
  def ppr = 
    if (isTuple) pprTuple(allArgs) else
    if (isList) group(between("[",arg.ppr,"]")) else
    if (isFun) {
      val (left,right) = allArgs match {
        case List(f@IfaceAppTy(_,_),x) if f.isFun => (par(f.ppr),x.ppr)
        case List(f@IfacePolyTy(_,_),x) => (par(f.ppr),x.ppr)
        case List(f,x) => (f.ppr,x.ppr)
        case _ => sys.error("Illegal function args: %s" format allArgs)
      }
      group(left :/: "->" :/: right)
    } else
    arg match {
      case a@IfaceAppTy(_,_) if !a.isTuple => 
        group(fun.ppr :/: par(arg.ppr))
      case IfacePolyTy(_,_) =>
        group(fun.ppr :/: par(arg.ppr))
      case _ => 
        group(fun.ppr :/: arg.ppr) 
    } 
}

case class IfaceTcTy(val con:GCon) extends IfaceType { def ppr = con.ppr }
case class IfaceTvTy(val name:Name) extends IfaceType { def ppr = name.ppr }

class IfacePred(val n:Name, tys:List[IfaceType]) extends PrettyPrintable {
  def ppr = group(n.ppr :/: pprMany(tys))
}

object IfaceType {
  @tailrec
  def unwind(
      ty:IfaceType, 
      args:List[IfaceType] = Nil):(IfaceType,List[IfaceType]) = ty match {
    case IfaceAppTy(fun,arg) => unwind(fun,arg::args)
    case _ => (ty,args)
  }
  
  def mkApp(fun:IfaceType, args:List[IfaceType]) = 
    args.foldLeft(fun)(IfaceAppTy(_,_))
    
  def mkFun(tys:List[IfaceType]) = 
    tys.reduceRight((x,y) => IfaceAppTy(IfaceAppTy(IfaceTcTy(ArrowCon),x),y))
}
