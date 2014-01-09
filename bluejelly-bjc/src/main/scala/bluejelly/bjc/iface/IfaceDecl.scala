/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.iface

import java.io.{DataInputStream,DataOutputStream}

import scala.text.Document.{empty,group,text}

import bluejelly.bjc.common.Name
import bluejelly.bjc.common.Binary._
import bluejelly.bjc.common.PprUtils._
import bluejelly.bjc.common.PrettyPrintable
import bluejelly.bjc.common.{Binary,Loadable,Serializable}
import bluejelly.bjc.ast.decls.FunDep

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
case class IfaceRecSelId(val tycon:Name) extends IfaceIdDetails {
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
abstract class IfaceDecl(val name:Name) 
  extends PrettyPrintable with Serializable

case class IfaceId(
    override val name:Name, 
    val ty:IfaceType,
    val details:IfaceIdDetails) extends IfaceDecl(name) {
  
  override def equals(o:Any) = o match {
    case x:IfaceId => x.name equals name
    case _ => false
  }
  
  override def hashCode = name.hashCode
  
  def ppr = gnest(cat(group(name.ppr :/: text("::")) :/: ty.ppr, details.ppr))
  
  def serialize(out:DataOutputStream) {
    out.writeByte(0)
    name.serialize(out)
    ty.serialize(out)
    details.serialize(out)
  }
}

case class IfaceTyCon(
    override val name:Name, 
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
    override val name:Name, 
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
    override val name:Name, 
    val vars:List[IfaceTyVar],
    val ctx:List[IfacePred],
    val pred:IfacePred,
    val fdeps:List[FDep],
    val ops:List[IfaceClsOp]) extends IfaceDecl(name) {
  def ppr = {
    val dctx = if (ctx.isEmpty) empty else group(
        (if (ctx.length == 1) ctx.head.ppr else pprTuple(ctx)) :/: text("=>"))
    val rule = cat(dctx, pred.ppr)
    val fds = if (fdeps.isEmpty) empty else gnest("|" :/: pprMany(fdeps,","))
    val w = if (ops.isEmpty) empty else gnest("where" :/: pprBlock(ops))
    gnest(cat(gnest(cat(gnest("class" :/: rule), fds)), w))
  }
  
  def serialize(out:DataOutputStream) {
    out.writeByte(3)
    name.serialize(out)
    vars.serialize(out)
    ctx.serialize(out)
    pred.serialize(out)
    fdeps.serialize(out)
    ops.serialize(out)
  }
}
    
/**
 * A functional dependency that can be dumped to a binary stream.
 * @author ppedemon
 */
class FDep(from:List[Name], to:List[Name]) 
    extends FunDep(from, to) with Serializable {
  def serialize(out:DataOutputStream) {
    from.serialize(out)
    to.serialize(out)
  }
}

/**
 * An interface data constructor.
 * @author ppedemon
 */
class IfaceDataCon(
    val name:Name, 
    val ty:IfaceType, 
    fields:List[Name], 
    stricts:List[Boolean]) extends PrettyPrintable with Serializable {
  def ppr = {
    val xs = stricts map {if (_) text("!") else text("_")}
    val sd = if (!stricts.isEmpty) 
      gnest("Stricts:" :/: group(cat(xs))) else empty
    val fd = if (!fields.isEmpty) 
      gnest("Fields:" :/: pprMany(fields)) else empty
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
    val name:Name, 
    val ty:IfaceType, 
    val isDefault:Boolean) extends PrettyPrintable with Serializable {
  def ppr = {
    val id = Name.asId(name)
    val nd = if (isDefault) group(id.ppr :/: text("{-D-}")) else id.ppr
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
    val name:Name,
    val dfunId:Name) extends PrettyPrintable with Serializable {
  
  def ppr = gnest(group("instance" :/: name.ppr :/: text("=")) :/: dfunId.ppr)
  
  def serialize(out:DataOutputStream) { 
    name.serialize(out) 
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
      new IfaceId(Name.load(in), IfaceType.load(in), loadIfaceIdDetails(in))
    case 1 =>
      new IfaceTyCon(
          Name.load(in), 
          Binary.loadList(IfaceType.loadPred, in), 
          Binary.loadList(IfaceType.loadTyVar, in), 
          Binary.loadList(loadIfaceDataCon, in))
    case 2 =>
      new IfaceTySyn(
          Name.load(in), 
          Binary.loadList(IfaceType.loadTyVar, in), 
          IfaceType.load(in))
    case 3 =>
      new IfaceCls(
          Name.load(in), 
          Binary.loadList(IfaceType.loadTyVar, in), 
          Binary.loadList(IfaceType.loadPred, in), 
          IfaceType.loadPred(in),
          Binary.loadList(loadFDep, in),
          Binary.loadList(loadIfaceClsOp, in))
  }
  
  private def loadIfaceIdDetails(in:DataInputStream):IfaceIdDetails =
    in.readByte match {
      case 0 => IfaceVanillaId
      case 1 => IfaceDFunId
      case 2 => IfaceRecSelId(Name.load(in))
    }
  
  private def loadFDep(in:DataInputStream) =
    new FDep(Binary.loadList(Name.load, in),Binary.loadList(Name.load, in))
  
  private def loadIfaceDataCon(in:DataInputStream) =
    new IfaceDataCon(
        Name.load(in), 
        IfaceType.load(in), 
        Binary.loadList(Name.load, in), 
        Binary.loadList(Binary.loadBoolean, in))
  
  private def loadIfaceClsOp(in:DataInputStream) = 
    new IfaceClsOp(Name.load(in), IfaceType.load(in), Binary.loadBoolean(in))
  
  private[iface] def loadIfaceClsInst(in:DataInputStream) = 
    new IfaceClsInst(Name.load(in), Name.load(in))
}
