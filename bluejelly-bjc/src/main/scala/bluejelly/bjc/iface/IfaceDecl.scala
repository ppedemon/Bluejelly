/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.iface

import java.io.{DataInputStream,DataOutputStream}

import bluejelly.bjc.common.TcRef
import bluejelly.bjc.common.{Name,QualName}
import bluejelly.bjc.common.Binary._
import bluejelly.bjc.common.PprUtils._
import bluejelly.bjc.common.PrettyPrintable
import bluejelly.bjc.common.{Binary,Loadable,Serializable}
import bluejelly.bjc.common.PrettyPrintable

import bluejelly.utils.Document.{empty,group,text}

/**
 * Description for a IfaceId.
 * @author ppedemon
 */
trait IfaceIdDetails extends PrettyPrintable with Serializable

case object IfaceVanillaId extends IfaceIdDetails { 
  def ppr = empty
  def serialize(out:DataOutputStream) { out.writeByte(0) }
}
case object IfaceDFunId extends IfaceIdDetails { 
  def ppr = text("{- DFunId -}")
  def serialize(out:DataOutputStream) { out.writeByte(1) }
}
case class IfaceRecSelId(val tycon:Symbol) extends IfaceIdDetails {
  def ppr = group(between("{- ", "RSel " :: tycon.ppr, " -}"))
  def serialize(out:DataOutputStream) {
    out.writeByte(2)
    tycon.serialize(out)
  }
}

/**
 * Interface top-level declarations.
 * @author ppedemon
 */
abstract class IfaceDecl(val name:Symbol) 
  extends PrettyPrintable with Serializable

case class IfaceId(
    override val name:Symbol, 
    val ty:IfaceType,
    val details:IfaceIdDetails) extends IfaceDecl(name) {
  
  override def equals(o:Any) = o match {
    case x:IfaceId => x.name equals name
    case _ => false
  }
  
  override def hashCode = name.hashCode
  
  def ppr = gnest(
    cat(group(name.ppr :/: text("::")) :/: ty.ppr, details.ppr))
  
  def serialize(out:DataOutputStream) {
    out.writeByte(0)
    name.serialize(out)
    ty.serialize(out)
    details.serialize(out)
  }
}

case class IfaceTyCon(
    override val name:Symbol,
    val ctx:List[IfacePred],
    val vars:List[IfaceTyVar], 
    val cons:List[IfaceDataCon]) extends IfaceDecl(name) {
  def ppr = {
    val dctx = if (ctx.isEmpty) empty else group(
      (if (ctx.length == 1) ctx.head.ppr else pprTuple(ctx)) :/: text("=>"))
    val dlhs = gnest(cat(List(
      text("data"), 
      dctx, 
      if (vars.isEmpty) name.ppr else group(name.ppr :/: pprMany(vars)),
      if (cons.isEmpty) empty else text("="))))
    gnest(cat(dlhs, pprMany(cons, " |")))
  }
  
  def serialize(out:DataOutputStream) {
    out.writeByte(1)
    name.serialize(out)
    ctx.serialize(out)
    vars.serialize(out)
    cons.serialize(out)
  }
}

case class IfaceTySyn(
    override val name:Symbol, 
    val vars:List[IfaceTyVar],
    val ty:IfaceType) extends IfaceDecl(name) {
  def ppr = gnest("type" :/: 
    group(cat(name.ppr,pprMany(vars)) :/: text("=")) :/: ty.ppr)
    
  def serialize(out:DataOutputStream) {
    out.writeByte(2)
    name.serialize(out)
    vars.serialize(out)
    ty.serialize(out)
  }
}

case class IfaceCls(
    override val name:Symbol, 
    val vars:List[IfaceTyVar],
    val ctx:List[IfacePred],
    val ops:List[IfaceClsOp]) extends IfaceDecl(name) {
  def ppr = {
    val dctx = if (ctx.isEmpty) empty else group(
        (if (ctx.length == 1) ctx.head.ppr else pprTuple(ctx)) :/: text("=>"))
    val rule = cat(dctx, group(name.ppr :/: pprMany(vars)))
    val w = if (ops.isEmpty) empty else gnest("where" :/: pprBlock(ops))
    gnest(cat(gnest("class" :/: rule), w))
  }
  
  def serialize(out:DataOutputStream) {
    out.writeByte(3)
    name.serialize(out)
    vars.serialize(out)
    ctx.serialize(out)
    ops.serialize(out)
  }
}

/**
 * An interface data constructor.
 * @author ppedemon
 */
class IfaceDataCon(
    val name:Symbol, 
    val ty:IfaceType, 
    val fields:List[Symbol], 
    val stricts:List[Boolean]) extends PrettyPrintable with Serializable {
  def ppr = {
    val xs = stricts map {if (_) text("!") else text("_")}
    val sd = if (!stricts.isEmpty) 
      gnest("Stricts:" :/: group(cat(xs))) else empty
    val fd = if (!fields.isEmpty) {
      val ps = fields map {new PrettyPrintableSymbol(_)}
      gnest("Fields:" :/: pprMany(ps)) 
    } else empty
    gnest(cat(List(group(name.ppr :/: text("::")), ty.ppr, sd, fd)))
  }
  
  def serialize(out:DataOutputStream) {
    name.serialize(out)
    ty.serialize(out)
    fields.serialize(out)
    stricts.serialize(out)
  }
}

/**
 * An interface class operation.
 * @author ppedemon
 */
class IfaceClsOp(
    val name:Symbol, 
    val ty:IfaceType, 
    val isDefault:Boolean) extends PrettyPrintable with Serializable {
  def ppr = {
    val nd = if (isDefault) 
      group(name.pprAsId :/: text("{-D-}")) else name.pprAsId
    gnest(group(nd :/: text("::")) :/: ty.ppr)
  }
  
  def serialize(out:DataOutputStream) {
    name.serialize(out)
    ty.serialize(out)
    isDefault.serialize(out)
  }
}

/**
 * An interface instance definition.
 * @author ppedemon
 */
class IfaceClsInst(
    val name:QualName,
    val con:TcRef,
    val dfunId:Symbol) extends PrettyPrintable with Serializable {
  
  def ppr = {
    gnest(group("instance" :/: name.ppr :/: 
      con.ppr :/: text("=")) :/: dfunId.ppr)
  }
  
  def serialize(out:DataOutputStream) { 
    name.serialize(out)
    con.serialize(out)
    dfunId.serialize(out) 
  }
}

/**
 * Common functionality for interface declarations.
 * @author ppedemon
 */
object IfaceDecl extends Loadable[IfaceDecl] {
  
  def load(in:DataInputStream) = in.readByte match {
    case 0 => 
      new IfaceId(
        Binary.loadSymbol(in), 
        IfaceType.load(in), 
        loadIfaceIdDetails(in))
    case 1 =>
      new IfaceTyCon(
          Binary.loadSymbol(in), 
          Binary.loadList(IfaceType.loadPred, in), 
          Binary.loadList(IfaceType.loadTyVar, in), 
          Binary.loadList(loadIfaceDataCon, in))
    case 2 =>
      new IfaceTySyn(
          Binary.loadSymbol(in), 
          Binary.loadList(IfaceType.loadTyVar, in), 
          IfaceType.load(in))
    case 3 =>
      new IfaceCls(
          Binary.loadSymbol(in), 
          Binary.loadList(IfaceType.loadTyVar, in), 
          Binary.loadList(IfaceType.loadPred, in),
          Binary.loadList(loadIfaceClsOp, in))
  }
  
  private def loadIfaceIdDetails(in:DataInputStream):IfaceIdDetails =
    in.readByte match {
      case 0 => IfaceVanillaId
      case 1 => IfaceDFunId
      case 2 => IfaceRecSelId(Binary.loadSymbol(in))
    }
    
  private def loadIfaceDataCon(in:DataInputStream) =
    new IfaceDataCon(
        Binary.loadSymbol(in), 
        IfaceType.load(in), 
        Binary.loadList(Binary.loadSymbol, in), 
        Binary.loadList(Binary.loadBoolean, in))
  
  private def loadIfaceClsOp(in:DataInputStream) = 
    new IfaceClsOp(
      Binary.loadSymbol(in), 
      IfaceType.load(in), 
      Binary.loadBoolean(in))
  
  private[iface] def loadIfaceClsInst(in:DataInputStream) = 
    new IfaceClsInst(
        Name.loadQual(in),
        TcRef.load(in), 
        Binary.loadSymbol(in))
}
