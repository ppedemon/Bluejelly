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
case class RecSelId(val tycon:TyCon) extends IdDetails
case class ClsOpId(val cls:Cls) extends IdDetails
case class DFunId(val inst:Inst) extends IdDetails

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
    val dcons:List[DataCon])

/**
 * Data constructors. If this is a record, the fields list will hold
 * a list of selection functions. Othewrwise, the list will be empty.
 *
 * @author ppedemon
 */
class DataCon(
    val name:Name,
    val ty:Type,
    val stricts:List[Boolean],
    val fields:List[Id],
    var tycon:TyCon = null)

/**
 * A class operation.
 * @author ppedemon
 */
case class ClsOp(val id:Id, val cls:Cls, val default:Boolean)

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
