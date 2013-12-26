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

/**
 * Interface top-level declarations.
 * @author ppedemon
 */
abstract class IfaceDecl extends PrettyPrintable

case class IfaceId(val name:Name, val ty:IfaceType) extends IfaceDecl {
  def ppr = gnest(group(name.ppr :/: text("::")) :/: ty.ppr)
}

case class IfaceTyCon(
    val name:Name, 
    val ctx:List[IfacePred],
    val vars:List[IfaceTyVar], 
    val cons:List[IfaceDataCon]) extends IfaceDecl {
  def ppr = {
    val dctx = if (ctx.isEmpty) empty else group(
      (if (ctx.length == 1) ctx.head.ppr else pprTuple(ctx)) :/: text("=>"))
    val dlhs = gnest(cat(List(
      text("data"), 
      dctx, 
      group(name.ppr :/: pprMany(vars)),
      if (cons.isEmpty) empty else text("="))))
    gnest(cat(dlhs, pprMany(cons, "|")))
  } 
}

case class IfaceTySyn(
    val name:Name, 
    val vars:List[IfaceTyVar],
    val ty:IfaceType) extends IfaceDecl {
  def ppr = gnest("type" :/: 
    group(name.ppr :/: pprMany(vars) :/: text("=")) :/: ty.ppr)
}

case class IfaceCls(
    val name:Name, 
    val ty:IfacePolyTy,
    val fdeps:List[FDep],
    val ops:List[IfaceClsOp]) extends IfaceDecl {
  def ppr = {
    val fds = if (fdeps.isEmpty) empty else gnest("|" :/: pprMany(fdeps,","))
    val w = if (ops.isEmpty) empty else gnest("where" :/: pprBlock(ops))
    gnest(cat(List(gnest("class" :/: name.ppr :/: ty.ty.ppr), fds, w)))
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
    val xs = stricts map {if (_) text("!") else text("L")}
    val sd = if (!stricts.isEmpty) 
      gnest("Stricts:" :/: group(cat(xs))) else empty
    val fd = if (!fields.isEmpty) 
      gnest("Fields:" :/: pprMany(fields)) else empty
    cat(List(group(name.ppr :/: text("::")), ty.ppr, sd, fd))
  }
}

/**
 * An interface class operation.
 * @author ppedemon
 */
class IfaceClsOp(
    val name:Name, 
    val ty:IfacePolyTy, 
    val isDefault:Boolean) extends PrettyPrintable {
  def ppr = {
    val nd = if (isDefault) group(name.ppr :/: text("{D}")) else name.ppr
    gnest(group(nd :/: text("::")) :/: ty.ppr)
  }
}

/**
 * An interface instance definition.
 * @author ppedemon
 */
class IfaceClsInst(
    val name:Name,
    val ty:IfaceType,
    val dfunId:Name) extends PrettyPrintable {
  def ppr = gnest(gnest("instance" :/: ty.ppr :/: text("=")) :/: dfunId.ppr)
}
