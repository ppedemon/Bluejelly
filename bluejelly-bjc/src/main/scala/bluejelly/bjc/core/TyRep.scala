/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */

package bluejelly.bjc.core

import bluejelly.bjc.ast.{GCon,UnitCon,TupleCon,ArrowCon,ListCon,Con}

import bluejelly.bjc.common.Name
import bluejelly.bjc.common.PprUtils._
import bluejelly.bjc.common.PrettyPrintable
import bluejelly.bjc.common.ScopedName
import bluejelly.bjc.common.ScopedName.{tvName}

import bluejelly.utils.Document.{text,group}

import scala.annotation.tailrec

/**
 * Kinds attached to type variables.
 * @author ppedemon
 */
trait Kind extends PrettyPrintable

case object KStar extends Kind { 
  def ppr = text("*") 
}

case class KVar(val name:Name) extends Kind{
  def ppr = name.ppr
}

case class KFun(val from:Kind, val to:Kind) extends Kind {
  def ppr = from match {
    case KFun(_,_) => gnest(par(from.ppr) :/: "->" :/: to.ppr)
    case _ => gnest(from.ppr :/: "->" :/: to.ppr)
  } 
}

/**
 * Type variables.
 * @author ppedemon
 */
class TyVar(val name:ScopedName, val kind:Kind) extends PrettyPrintable {
  def ppr = kind match {
    case KStar => name.ppr
    case _ => par(group(name.ppr :: "::" :: kind.ppr))
  }
}

/**
 * Type predicates. Predicates reference type classes by name,
 * meaning that will have to lookup the name in the corresponding 
 * environment to get the corresponding TypeClass object.
 *
 * @author ppedemon
 */
class TyPred(val n:Name, tys:List[Type]) extends PrettyPrintable {  
  def ppr = group(n.ppr :/: pprMany(tys))
}

/**
 * Type representation. This will be the type model that we
 * ill use for type inference. Interfaces loaded at compile
 * time will be translated to this representation.
 *
 * Types will reference type constructors by name (i.e., you
 * will have to lookup the name in the environment to get the
 * corresponding TyCon object).
 * 
 * @author ppedemon
 */
trait Type extends PrettyPrintable

case class PolyTy(val tvs:List[TyVar], val ty:Type) extends Type {
  def ppr = if (tvs.isEmpty) ty.ppr else 
    gnest(gnest("forall" :/: pprMany(tvs) :/: text(".")) :/: ty.ppr)
} 

case class QualTy(val ctx:List[TyPred], val ty:Type) extends Type {
  def ppr = {
    val dctx = group((if (ctx.length == 1) 
      ctx.head.ppr else pprTuple(ctx)) :/: text("=>"))
    gnest(dctx:/: ty.ppr)
  } 
}

case class AppTy(val fun:Type, val arg:Type) extends Type {
  lazy val (head,allArgs) = Type.unwind(this)

  def isTuple = head match {
    case TcTy(TupleCon(_)) => true
    case _ => false
  }
  def isFun = head match {
    case TcTy(ArrowCon) => true
    case _ => false
  }
  def isList = fun match {
    case TcTy(ListCon) => true
    case _ => false
  }
  
  def ppr = 
    if (isTuple) pprTuple(allArgs) else
    if (isList) group(between("[",arg.ppr,"]")) else
    if (isFun) {
      val (left,right) = allArgs match {
        case List(f@AppTy(_,_),x) if f.isFun => (par(f.ppr),x.ppr)
        case List(f@PolyTy(_,_),x) => (par(f.ppr),x.ppr)
        case List(f,x) => (f.ppr,x.ppr)
        case _ => sys.error("Illegal function args: %s" format allArgs)
      }
      group(gnest(left :/: text("->")) :/: right)
    } else
    arg match {
      case a@AppTy(_,_) if !a.isTuple && !a.isList => 
        group(fun.ppr :/: par(arg.ppr))
      case PolyTy(_,_) =>
        group(fun.ppr :/: par(arg.ppr))
      case _ => 
        group(fun.ppr :/: arg.ppr) 
    } 
}

case class TcTy(val con:GCon) extends Type { 
  def ppr = con.ppr 
}

case class TvTy(val name:ScopedName) extends Type { 
  def ppr = name.ppr
}

object Type {
  @tailrec
  def unwind(ty:Type, args:List[Type] = Nil):(Type,List[Type]) = ty match {
    case AppTy(fun,arg) => unwind(fun,arg::args)
    case _ => (ty,args)
  }
  
  def mkApp(fun:Type, args:List[Type]) = 
    args.foldLeft(fun)(AppTy(_,_))
  
  def mkFun(from:Type, to:Type) = 
    AppTy(AppTy(TcTy(ArrowCon),from),to)  
    
  def mkFun(tys:List[Type]) = 
    tys.reduceRight((x,y) => AppTy(AppTy(TcTy(ArrowCon),x),y)) 

  def tyVar(n:Symbol, kind:Kind=KStar) = new TyVar(tvName(n),kind)
  def tvTy(n:Symbol) = new TvTy(tvName(n))
  def conTy(n:Name) = new TcTy(Con(n))
}
