/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.core

import bluejelly.bjc.common.Name
import scala.util.parsing.input.Position

/**
 * Classify identifier declarations.
 * @author ppedemon
 */
abstract class IdDetails
case object VanillaId extends IdDetails
case class RSelId(val tycon:TyCon) extends IdDetails
case class ClOpId(val cls:Cls) extends IdDetails
case class DFunId(val inst:Inst)

/**
 * A function, type class operation, or record selector.
 * @author ppedemon
 */
class Id(
    val name:Name,
    val arity:Int,
    val details:IdDetails,
    val ty:Type)

/**
 * A type synonym declaration.
 */
class TySyn(val name:Name, val tyvars:List[TyVar], kind:Kind, ty:Type)

/**
 * Type constructors.
 * @author ppedemon
 */
class TyCon(
    val name:Name,
    val newType:Boolean,
    val ctx:List[TyPred],
    val tyvars:List[TyVar],
    val kind:Kind,
    val dcons:List[DataCon])

/**
 * Data constructor field: it has a type, a flag denoting its strictness,
 * and an optional selector name if the data constructor is a record.
 *
 * @author ppedemon
 */
case class Field(val ty:Type, val strict:Boolean, op:Option[Id])

/**
 * Data constructors.
 * @author ppedemon
 */
class DataCon(
    val name:Name,
    val ty:Type,
    val fields:List[Field],
    tycon:TyCon)

/**
 * A class operation.
 * @author ppedemon
 */
case class ClsOp(val op:Id, val cls:Cls, val default:Boolean)

/**
 * A class declaration.
 * @author ppedemon
 */
class Cls(
    val name:Name,
    val tyvar:TyVar,
    val ctx:List[TyPred],
    val pred:TyPred,
    val ops:List[ClsOp])

/**
 * An instance declaration.
 * @author ppedemon
 */
class Inst(val cls:Cls, val constr:TyCon, val dfunId:Id)
