/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.core

import bluejelly.bjc.ast.{GCon}
import bluejelly.bjc.ast.decls.{Assoc,NoAssoc,LeftAssoc,RightAssoc}

import bluejelly.bjc.common.{ExportInfo,ExportedId,ExportedTc}
import bluejelly.bjc.common.Fixity
import bluejelly.bjc.common.Name
import bluejelly.bjc.common.PprUtils._
import bluejelly.bjc.common.PrettyPrintable
import bluejelly.bjc.common.ScopedName

import bluejelly.bjc.iface._

import bluejelly.utils.Document
import bluejelly.utils.Document.{empty,group,text}

/**
 * Classify identifier declarations.
 * @author ppedemon
 */
abstract class IdDetails extends PrettyPrintable
case object VanillaId extends IdDetails {
  def ppr = empty 
}
case class RecSelId(val tycon:TyCon) extends IdDetails {
  def ppr = group(between("{- ", "RSel " :: tycon.name.ppr, " -}"))
}
case class ClsOpId(val cls:Cls) extends IdDetails {
  def ppr = group(between("{- ", "Cls " :: cls.name.ppr, " -}"))
}
case class DFunId(val inst:Inst) extends IdDetails {
  def ppr = text("{- DFunId -}")
}

/**
 * An abstract module declaration.
 * @author ppedemon
 */
abstract class ModDecl(val name:ScopedName) extends PrettyPrintable {
  override def equals(x:Any) = x match {
    case other:ModDecl => other.name == name
    case _ => false
  }
  override def hashCode = name.hashCode
}

/**
 * A function, type class operation, or record selector.
 * @author ppedemon
 */
case class Id(
    override val name:ScopedName,
    var details:IdDetails,
    val ty:Type) extends ModDecl(name) {

  def ppr = gnest(cat(group(name.ppr :/: text("::")) :/: ty.ppr, details.ppr))
}

/**
 * A type synonym declaration.
 */
case class TySyn(
    override val name:ScopedName,
    val tyvars:List[TyVar],  
    ty:Type) extends ModDecl(name) {

  def ppr = gnest("type" :/: 
    group(cat(name.ppr,pprMany(tyvars)) :/: text("=")) :/: ty.ppr)
}

/**
 * Type constructors.
 * @author ppedemon
 */
case class TyCon(
    override val name:ScopedName,
    val ctx:List[TyPred],
    val tyvars:List[TyVar],
    val dcons:List[DataCon]) extends ModDecl(name) {

  def ppr = {
    val dctx = if (ctx.isEmpty) empty else group(
      (if (ctx.length == 1) ctx.head.ppr else pprTuple(ctx)) :/: text("=>"))
    val dlhs = gnest(cat(List(
      text("data"), 
      dctx, 
      if (tyvars.isEmpty) name.ppr else group(name.ppr :/: pprMany(tyvars)),
      if (dcons.isEmpty) empty else text("="))))
    gnest(cat(dlhs, pprMany(dcons, " |")))
  }
}

/**
 * Data constructors. If this is a record, the fields list will hold
 * a list of selection functions. Othewrwise, the list will be empty.
 *
 * @author ppedemon
 */
case class DataCon(
    override val name:ScopedName,
    val ty:Type,
    val stricts:List[Boolean],
    val fields:List[Id],
    val tag:Int,
    var tycon:TyCon = null) extends ModDecl(name) {

  def ppr = {
    val xs = stricts map {if (_) text("!") else text("_")}
    val sd = if (!stricts.isEmpty) 
      gnest("Stricts:" :/: group(cat(xs))) else empty
    val fd = if (!fields.isEmpty) 
      gnest("Fields:" :/: pprMany(fields.map(_.name))) else empty
    gnest(cat(List(group(name.ppr :/: text("::")), ty.ppr, sd, fd)))
  }
}

/**
 * A class operation.
 * @author ppedemon
 */
case class ClsOp(
    val id:Id, 
    var cls:Cls, 
    val default:Boolean) extends PrettyPrintable {
  
  def ppr = {
    val nm = id.name.pprAsId
    val nd = if (default) group(nm :/: text("{-D-}")) else nm
    gnest(group(nd :/: text("::")) :/: id.ty.ppr)
  } 
}

/**
 * A class declaration.
 * @author ppedemon
 */
case class Cls(
    override val name:ScopedName,
    val tyvar:TyVar,
    val ctx:List[TyPred],
    val ops:List[ClsOp]) extends ModDecl(name) {

  def ppr = {
    val dctx = if (ctx.isEmpty) empty else group(
        (if (ctx.length == 1) ctx.head.ppr else pprTuple(ctx)) :/: text("=>"))
    val rule = cat(dctx, group(name.ppr :/: tyvar.ppr))
    val w = if (ops.isEmpty) empty else gnest("where" :/: pprBlock(ops))
    gnest(cat(gnest("class" :/: rule), w))
  }
}

/**
 * An instance declaration.
 * @author ppedemon
 */
class Inst(
    val clsName:Name, 
    val con:GCon, 
    val dfunId:Id) extends PrettyPrintable {

  def ppr = gnest(
    group("instance" :/: clsName.ppr :/: text("=")) :/: dfunId.ppr)
}

/**
 * An module definition.
 * @author ppedemon
 */
 class ModDefn(
    val name:Symbol,
    val exports:List[ExportInfo],
    val fixities:Map[ScopedName,Fixity],
    val decls:Map[ScopedName, ModDecl],
    val insts:List[Inst]) extends PrettyPrintable {

  def this(name:Symbol) = 
    this(name, List.empty, Map.empty, Map.empty, List.empty)

  def this(name:Symbol, exports:List[ExportInfo]) = 
    this(name, exports, Map.empty, Map.empty, List.empty) 

  def this(name:Symbol, exports:List[ExportInfo], fixities:Map[ScopedName,Fixity]) =
    this(name, exports, fixities, Map.empty, List.empty)

  def ppr = {
    val fds = fixities.foldRight(List.empty[PrettyPrintable])((p,ps) => 
      new PrettyPrintable {def ppr = group(p._2.ppr :/: p._1.pprAsOp)}::ps)
    val fd = if (fixities.isEmpty) empty else 
      nl::gnest("Fixities:" :/: pprMany(fds, ","))
    val ed = if (exports.isEmpty) empty else 
      nl::gnest("Exports:" :/: pprMany(exports))
    cat(List(
      group("module" :/: name.ppr :/: text("where")), 
      ed, fd,
      if (decls.isEmpty) empty else nl::vppr(decls.values.toList),
      if (insts.isEmpty) empty else nl::vppr(insts.toList)))
  }

  // ---------------------------------------------------------------------
  // Grow a ModDefn instance
  // ---------------------------------------------------------------------

  def addExport(export:ExportInfo) = 
    new ModDefn(name, export::exports, fixities, decls, insts)

  def addFixity(op:ScopedName,fixity:Fixity) = 
    new ModDefn(name, exports, fixities + (op -> fixity), decls, insts)

  def addId(id:Id) = 
    new ModDefn(name, exports, fixities, addDecl(decls, id), insts)

  def addTySyn(tysyn:TySyn) = 
    new ModDefn(name, exports, fixities, addDecl(decls, tysyn), insts)

  def addTyCon(tycon:TyCon) = {
    val n_decls = addDecls(decls, tycon::tycon.dcons)
    new ModDefn(name, exports, fixities, n_decls, insts)
  }

  def addClass(cls:Cls) = {
    val n_decls = addDecls(decls, cls::cls.ops.map(_.id))
    new ModDefn(name, exports, fixities, n_decls, insts)
  }

  def addInst(inst:Inst) =
    new ModDefn(name, exports, fixities, decls, inst::insts)

  def getId:PartialFunction[ScopedName,Id] = decls(_) match {
    case id:Id => id
  }

  def getTyCon:PartialFunction[ScopedName,TyCon] = decls(_) match {
    case tc:TyCon => tc
  }

  def getTySyn:PartialFunction[ScopedName,TySyn] = decls(_) match {
    case ts:TySyn => ts
  }

  def getCls:PartialFunction[ScopedName,Cls] = decls(_) match {
    case cl:Cls => cl
  }

  def getDataCon:PartialFunction[ScopedName,DataCon] = decls(_) match {
    case dc:DataCon => dc
  }

  // ---------------------------------------------------------------------
  // Helper private functions: 
  //  grow a ModDefn with one or more ModDecl instances
  // ---------------------------------------------------------------------

  private def addDecl(m:Map[ScopedName,ModDecl],d:ModDecl) = 
    m + (d.name -> d)
 
  private def addDecls(m:Map[ScopedName,ModDecl], ds:List[ModDecl]) = 
    ds.foldLeft(this.decls)(addDecl)
}
