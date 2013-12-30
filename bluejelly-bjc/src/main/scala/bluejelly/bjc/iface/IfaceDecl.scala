/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.iface

import scala.text.Document.{empty,group,text}

import bluejelly.bjc.common.Name
import bluejelly.bjc.common.PprUtils._
import bluejelly.bjc.common.PrettyPrintable
import bluejelly.bjc.ast.decls.FunDep

trait IfaceIdDetails extends PrettyPrintable
case object IfaceVanillaId extends IfaceIdDetails { def ppr = empty }
case object IfaceDFund extends IfaceIdDetails { def ppr = text("{- DFunId -}")}
case class IfaceRecSelId(val tycon:Name) extends IfaceIdDetails {
  def ppr = group(between("{- ", "RSel " :: tycon.ppr, " -}"))
}

/**
 * Interface top-level declarations.
 * @author ppedemon
 */
abstract class IfaceDecl(val name:Name) extends PrettyPrintable

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
}

case class IfaceTySyn(
    override val name:Name, 
    val vars:List[IfaceTyVar],
    val ty:IfaceType) extends IfaceDecl(name) {
  def ppr = gnest("type" :/: 
    group(cat(name.ppr,pprMany(vars)) :/: text("=")) :/: ty.ppr)
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
}
    
/**
 * A functional dependency that can be dumped to a binary stream.
 * @author ppedemon
 */
class FDep(from:List[Name], to:List[Name]) extends FunDep(from, to)    

/**
 * An interface data constructor.
 * @author ppedemon
 */
class IfaceDataCon(
    val name:Name, 
    val ty:IfaceType, 
    fields:List[Name], 
    stricts:List[Boolean]) extends PrettyPrintable {
  def ppr = {
    val xs = stricts map {if (_) text("!") else text("_")}
    val sd = if (!stricts.isEmpty) 
      gnest("Stricts:" :/: group(cat(xs))) else empty
    val fd = if (!fields.isEmpty) 
      gnest("Fields:" :/: pprMany(fields)) else empty
    gnest(cat(List(group(name.ppr :/: text("::")), ty.ppr, sd, fd)))
  }
}

/**
 * An interface class operation.
 * @author ppedemon
 */
class IfaceClsOp(
    val name:Name, 
    val ty:IfaceType, 
    val isDefault:Boolean) extends PrettyPrintable {
  def ppr = {
    val id = Name.asId(name)
    val nd = if (isDefault) group(id.ppr :/: text("{D}")) else id.ppr
    gnest(group(nd :/: text("::")) :/: ty.ppr)
  }
}

/**
 * An interface instance definition.
 * @author ppedemon
 */
class IfaceClsInst(
    val name:Name,
    val dfunId:Name) extends PrettyPrintable {
  def ppr = gnest(group("instance" :/: name.ppr :/: text("=")) :/: dfunId.ppr)
}
