/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.core

import bluejelly.bjc.ast.{GCon}
import bluejelly.bjc.ast.decls.{Assoc,NoAssoc,LeftAssoc,RightAssoc}

import bluejelly.bjc.common.Name
import bluejelly.bjc.common.PprUtils._
import bluejelly.bjc.common.PrettyPrintable

import bluejelly.bjc.iface.{ModIface,IfaceExport,ExportedId,ExportedTc,Fixity}

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
abstract class ModDecl(val name:Name) extends PrettyPrintable

/**
 * Trait for some decl belonging to the tycon scope, i.e.:
 * Type synonyms, Type constructors, or Classes.
 *
 * @author ppedemon
 */
trait TcDecl extends PrettyPrintable

/**
 * A function, type class operation, or record selector.
 * @author ppedemon
 */
case class Id(
    override val name:Name,
    var details:IdDetails,
    val ty:Type) extends ModDecl(name) {

  def ppr = gnest(cat(group(name.ppr :/: text("::")) :/: ty.ppr, details.ppr))
}

/**
 * A type synonym declaration.
 */
case class TySyn(
    override val name:Name,
    val tyvars:List[TyVar],  
    ty:Type) extends ModDecl(name) with TcDecl {

  def ppr = gnest("type" :/: 
    group(cat(name.ppr,pprMany(tyvars)) :/: text("=")) :/: ty.ppr)
}

/**
 * Type constructors.
 * @author ppedemon
 */
case class TyCon(
    override val name:Name,
    val ctx:List[TyPred],
    val tyvars:List[TyVar],
    val dcons:List[DataCon]) extends ModDecl(name) with TcDecl {

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
    override val name:Name,
    val ty:Type,
    val stricts:List[Boolean],
    val fields:List[Id],
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
    val nm = Name.asId(id.name)
    val nd = if (default) group(nm.ppr :/: text("{-D-}")) else nm.ppr
    gnest(group(nd :/: text("::")) :/: id.ty.ppr)
  } 
}

/**
 * A class declaration.
 * @author ppedemon
 */
case class Cls(
    override val name:Name,
    val tyvar:TyVar,
    val ctx:List[TyPred],
    val ops:List[ClsOp]) extends ModDecl(name) with TcDecl {

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
    val cls:Cls, 
    val gcon:GCon, 
    val dfunId:Id) extends PrettyPrintable {

  def ppr = gnest(
    group("instance" :/: cls.name.ppr :/: text("=")) :/: dfunId.ppr)
}

/**
 * An module definition.
 * @author ppedemon
 */
 class ModDefn(
    val name:Name,
    val exports:List[IfaceExport],
    val fixities:Map[Name,Fixity],
    val ids:Map[Name,Id],        // Top-level ids, record selectors, dfun ids
    val tcs:Map[Name,TcDecl],    // TySyns, TyCons, Classes
    val dcons:Map[Name,DataCon], // Data constructors
    val insts:Map[Name,List[Inst]]) extends PrettyPrintable {

  def ppr = {
    val fds = fixities.toList map Function.tupled((n,f) => new PrettyPrintable {
      def ppr = group(f.ppr :/: Name.asOp(n).ppr)
    })
    val fd = if (fixities.isEmpty) empty else 
      nl::gnest("Fixities:" :/: pprMany(fds, ","))
    val ed = if (exports.isEmpty) empty else 
      nl::gnest("Exports:" :/: pprMany(exports))
    cat(List(
      group("module" :/: name.ppr :/: text("where")), 
      ed, fd,
      if (ids.isEmpty) empty else nl :: vppr(ids.values.toList),
      if (tcs.isEmpty) empty else nl :: vppr(tcs.values.toList),
      if (insts.isEmpty) empty else nl :: vppr(insts.values.flatten.toList)))
  }

  def this(name:Name) = this(name, List.empty, 
    Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)

  def this(name:Name, exports:List[IfaceExport]) = this(name, exports,
    Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)

  def this(
    name:Name, 
    exports:List[IfaceExport], 
    fixities:Map[Name,Fixity],
    ids:Map[Name,Id]) = this(name, exports, fixities, ids, 
      Map.empty, Map.empty, Map.empty)

  def addExport(export:IfaceExport) = new ModDefn(name, export::exports, 
    fixities, ids, tcs, dcons, insts)

  def addFixity(name:Name,fixity:Fixity) = new ModDefn(name, exports, 
    fixities + (name -> fixity), ids, tcs, dcons, insts)  

  def addId(id:Id) = new ModDefn(name, exports, 
    fixities, ids + (id.name -> id), tcs, dcons, insts)

  def addTySyn(tysyn:TySyn) = new ModDefn(name, exports, 
    fixities, ids, tcs + (tysyn.name -> tysyn), dcons, insts)

  def addTyCon(tycon:TyCon) = {
    val n_dcons = dcons ++ tycon.dcons.map(dc => (dc.name,dc))
    new ModDefn(name, exports, 
      fixities, ids, tcs + (tycon.name -> tycon), n_dcons, insts)
  }

  def addClass(cls:Cls) = {
    val n_ids = ids ++ cls.ops.map(op => (op.id.name,op.id))
    new ModDefn(name, exports, 
      fixities, n_ids, tcs + (cls.name -> cls), dcons, insts)
  }

  def addInst(inst:Inst) = {
    val n_insts = insts + 
      (inst.cls.name -> (inst :: insts.get(inst.cls.name).getOrElse(Nil)))
    new ModDefn(name, exports, fixities, ids, tcs, dcons, n_insts)
  }

  def getTyCon:PartialFunction[Name,TyCon] = n => tcs(n) match {
    case t@TyCon(_,_,_,_) => t
  }
 
  def getCls:PartialFunction[Name,Cls] = n => tcs(n) match {
    case c@Cls(_,_,_,_) => c
  }
}
