/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.iface

import java.io.{DataInputStream,DataOutputStream}

import scala.annotation.tailrec

import bluejelly.bjc.ast.{GCon,UnitCon,TupleCon,ArrowCon,ListCon,Con}

import bluejelly.bjc.common.Binary._
import bluejelly.bjc.common.PprUtils._
import bluejelly.bjc.common.PrettyPrintable
import bluejelly.bjc.common.Name
import bluejelly.bjc.common.{Serializable,Loadable,Binary}

import bluejelly.utils.Document.{text,group}

/**
 * Interface kinds.
 * @author ppedemon
 */
trait IfaceKind extends PrettyPrintable with Serializable

case object IfaceKStar extends IfaceKind { 
  def ppr = text("*")
  def serialize(out:DataOutputStream) { out.writeByte(0) }
}

case class IfaceKFun(val from:IfaceKind, val to:IfaceKind) extends IfaceKind {
  def ppr = from match {
    case IfaceKFun(_,_) => gnest(par(from.ppr) :/: "->" :/: to.ppr)
    case _ => gnest(from.ppr :/: "->" :/: to.ppr)
  }
  
  def serialize(out:DataOutputStream) {
    out.writeInt(1)
    from.serialize(out)
    to.serialize(out)
  }
  
  override def equals(other:Any) = other match {
    case x:IfaceKFun => (x.from equals from) && (x.to equals to)
    case _ => false
  }
  
  override def hashCode = 17*from.hashCode + to.hashCode 
}

object IfaceKind extends Loadable[IfaceKind]{
  def load(in:DataInputStream):IfaceKind = in.readByte match {
    case 0 => IfaceKStar
    case 1 => IfaceKFun(load(in),load(in))
  }
}

/**
 * An interface type variable.
 * @author ppedemon
 */
class IfaceTyVar(
    val name:Name, 
    val kind:IfaceKind) extends PrettyPrintable with Serializable {
  def ppr = kind match {
    case IfaceKStar => name.ppr
    case _ => par(group(name.ppr :: "::" :: kind.ppr))
  }
  
  def serialize(out:DataOutputStream) {
    name.serialize(out)
    kind.serialize(out)
  }
  
  override def equals(other:Any) = other match {
    case x:IfaceTyVar => (x.name equals name) && (x.kind equals kind)
    case _ => false
  }
  
  override def hashCode = 17*name.hashCode + kind.hashCode
}

/**
 * Type information for things available in an interface file.
 * @author ppedemon
 */
trait IfaceType extends PrettyPrintable with Serializable

case class IfacePolyTy(
    val tvs:List[IfaceTyVar], 
    val ty:IfaceType) extends IfaceType {
  def ppr = if (tvs.isEmpty) ty.ppr else
    gnest(gnest("forall" :/: pprMany(tvs) :/: text(".")) :/: ty.ppr)
  
  def serialize(out:DataOutputStream) {
    out.writeByte(0)
    tvs.serialize(out)
    ty.serialize(out)
  }
} 

case class IfaceQualTy(
    val ctx:List[IfacePred], 
    val ty:IfaceType) extends IfaceType {
  def ppr = {
    val dctx = group(if (ctx.length == 1) 
      ctx.head.ppr else pprTuple(ctx) :/: text("=>"))
    gnest(dctx:/: ty.ppr)
  } 
    
  def serialize(out:DataOutputStream) {
    out.writeByte(1)
    ctx.serialize(out)
    ty.serialize(out)
  }
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
      group(gnest(left :/: text("->")) :/: right)
    } else
    arg match {
      case a@IfaceAppTy(_,_) if !a.isTuple && !a.isList => 
        group(fun.ppr :/: par(arg.ppr))
      case IfacePolyTy(_,_) =>
        group(fun.ppr :/: par(arg.ppr))
      case _ => 
        group(fun.ppr :/: arg.ppr) 
    } 
  
  def serialize(out:DataOutputStream) {
    out.writeByte(2)
    fun.serialize(out)
    arg.serialize(out)
  }
}

case class IfaceTcTy(val con:GCon) extends IfaceType { 
  def ppr = con.ppr 
  def serialize(out:DataOutputStream) { 
    out.writeByte(3)
    IfaceType.serializeGCon(con, out) 
  }
}

case class IfaceTvTy(val name:Name) extends IfaceType { 
  def ppr = name.ppr
  def serialize(out:DataOutputStream) { out.writeByte(4); name.serialize(out) }
}

class IfacePred(
    val n:Name, 
    val tys:List[IfaceType]) extends PrettyPrintable with Serializable {
  
  def ppr = group(n.ppr :/: pprMany(tys))
  
  def serialize(out:DataOutputStream) {
    n.serialize(out)
    tys.serialize(out)
  }
}

object IfaceType extends Loadable[IfaceType] {
  @tailrec
  def unwind(
      ty:IfaceType, 
      args:List[IfaceType] = Nil):(IfaceType,List[IfaceType]) = ty match {
    case IfaceAppTy(fun,arg) => unwind(fun,arg::args)
    case _ => (ty,args)
  }
  
  def mkApp(fun:IfaceType, args:List[IfaceType]) = 
    args.foldLeft(fun)(IfaceAppTy(_,_))
  
  def mkFun(from:IfaceType, to:IfaceType) = 
    IfaceAppTy(IfaceAppTy(IfaceTcTy(ArrowCon),from),to)  
    
  def mkFun(tys:List[IfaceType]) = 
    tys.reduceRight((x,y) => IfaceAppTy(IfaceAppTy(IfaceTcTy(ArrowCon),x),y))
  

  // ---------------------------------------------------------------------
  // Serialization stuff
  // ---------------------------------------------------------------------
  
  def load(in:DataInputStream):IfaceType = in.readByte match {
    case 0 => IfacePolyTy(Binary.loadList(loadTyVar, in), load(in))
    case 1 => IfaceQualTy(Binary.loadList(loadPred, in), load(in))
    case 2 => IfaceAppTy(load(in), load(in))
    case 3 => IfaceTcTy(loadGCon(in))
    case 4 => IfaceTvTy(Name.load(in))
  }

  private[iface] def loadTyVar(in:DataInputStream) =
    new IfaceTyVar(Name.load(in), IfaceKind.load(in))
  
  private[iface] def loadPred(in:DataInputStream) = 
    new IfacePred(Name.load(in), Binary.loadList(load, in))

  private[iface] def loadGCon(in:DataInputStream) = in.readByte match {
    case 0 => ListCon
    case 1 => ArrowCon
    case 2 => UnitCon
    case 3 => Con(Name.load(in))
    case 4 => TupleCon(in.readInt)
  }

  // We could do out.writeByte(3+n) for tuples, but that would artificially 
  // limit tuple size to 256 elements, we don't want that!
  private[iface] def serializeGCon(con:GCon, out:DataOutputStream) = con match {
    case ListCon     => out.writeByte(0)
    case ArrowCon    => out.writeByte(1)
    case UnitCon     => out.writeByte(2)
    case Con(n)      => out.writeByte(3); n.serialize(out)
    case TupleCon(n) => out.writeByte(4); out.writeInt(n)
  }  
}
