/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.core

import bluejelly.bjc.ast.decls.{Assoc,NoAssoc,LeftAssoc,RightAssoc}
import bluejelly.bjc.common.Name
import bluejelly.bjc.iface.{ModIface,IfaceExport,ExportedId,ExportedTc,Fixity}

/**
 * Classify identifier declarations.
 * @author ppedemon
 */
abstract class IdDetails
case object VanillaId extends IdDetails
case class RecSelId(val tycon:TyCon) extends IdDetails
case class ClsOpId(val cls:Cls) extends IdDetails
case class DFunId(val inst:Inst) extends IdDetails

/**
 * An abstract module declaration.
 * @author ppedemon
 */
abstract class ModDecl(val name:Name)

/**
 * Trait for some decl belonging to the tycon scope, i.e.:
 * Type synonyms, Type constructors, or Classes.
 *
 * @author ppedemon
 */
trait TcDecl

/**
 * A function, type class operation, or record selector.
 * @author ppedemon
 */
case class Id(
    override val name:Name,
    val arity:Int,
    val details:IdDetails,
    val ty:Type) extends ModDecl(name)

/**
 * A type synonym declaration.
 */
case class TySyn(
    override val name:Name,
    val tyvars:List[TyVar],  
    ty:Type) extends ModDecl(name) with TcDecl

/**
 * Type constructors.
 * @author ppedemon
 */
case class TyCon(
    override val name:Name,
    val newType:Boolean,
    val ctx:List[TyPred],
    val tyvars:List[TyVar],
    val dcons:List[DataCon]) extends ModDecl(name) with TcDecl

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
    var tycon:TyCon = null) extends ModDecl(name)

/**
 * A class operation.
 * @author ppedemon
 */
case class ClsOp(val id:Id, val cls:Cls, val default:Boolean)

/**
 * A class declaration.
 * @author ppedemon
 */
case class Cls(
    override val name:Name,
    val tyvar:TyVar,
    val ctx:List[TyPred],
    val pred:TyPred,
    val ops:List[ClsOp]) extends ModDecl(name) with TcDecl

/**
 * An instance declaration.
 * @author ppedemon
 */
class Inst(val cls:Cls, val constr:TyCon, val dfunId:Id)

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
    val insts:Map[Name,List[Inst]]) {
  
  def this(name:Name) = this(name, List.empty, 
    Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)

  def addExport(export:IfaceExport) = new ModDefn(name, export::exports, 
    fixities, ids, tcs, dcons, insts)

  def addFixity(name:Name,fixity:Fixity) = new ModDefn(name, exports, 
    fixities + (name -> fixity), ids, tcs, dcons, insts)  

  def addId(id:Id) = new ModDefn(name, exports, 
    fixities, ids + (id.name -> id), tcs, dcons, insts)

  def addTySyn(tysyn:TySyn) = new ModDefn(name, exports, 
    fixities, ids, tcs + (tysyn.name -> tysyn), dcons, insts)

  def addTyCon(tycon:TyCon) = {
    val n_ids = ids ++ tycon.dcons.flatMap(_.fields.map(id => (id.name,id)))
    val n_dcons = dcons ++ tycon.dcons.map(dc => (dc.name,dc))
    new ModDefn(name, exports, 
      fixities, n_ids, tcs + (tycon.name -> tycon), n_dcons, insts)
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
}
