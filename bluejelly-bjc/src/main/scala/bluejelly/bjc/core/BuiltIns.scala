/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.core

import bluejelly.bjc.ast.{GCon,UnitCon,TupleCon,ArrowCon,ListCon,Con}
import bluejelly.bjc.common.Name
import bluejelly.bjc.iface.{ExportedId,ExportedTc}

import Type._

/**
 * Information about built-in stuff. Here we provide built-in [[ModDefn]]
 * instances for the constructs natively provided by the language, such as 
 * lists or primitive types.
 *
 * We distinguish between *wired-in* stuff (special syuntax, not belonging
 * to a module) and primitive definitions (normal syntax, defined inside a 
 * module, referenciable in qualified or unqualified form)  
 *
 * @author ppedemon
 */
object BuiltIns {

  /**
   * Wired-in tycons: provided by the compiler with special syntax
   * (except for type bool). Even though they are defined in a module
   * named `bluejelly.Base' (everything has to belong to a module), 
   * they must be referenced in unqualified form in source code. So
   * the wired-in module needs special treatment when loaded to the
   * environment. Wired-in tycons:
   *  
   *   - arrow: data (->)
   *   - unit:  data ()
   *   - bool:  data Bool = False | True
   *   - lists: data [a] = [] | a:[a]
   *
   * Tuples (other than unit) are magic: since they are an infinite
   * family of tycons, they don't belong to `bluejelly.WiredIn'. The 
   * environment will have to handle them as a special case. But
   * they are still considered to be wired-in tycons. 
   */
  private val wiredInModName = Name(Symbol("bluejelly.Base"))

  private val nmArrow = Name('->)
  private val nmUnit = Name(Symbol("()"))
  private val nmList = Name(Symbol("[]"))
  private val nmCons = Name(':)
  private val nmBool = Name('Bool)
  private val nmTrue = Name('True)
  private val nmFalse = Name('False)

  def wiredInMod = {
    val wiredInTyCons = List(unitTyCon, arrowTyCon, boolTyCon, listTyCon)
    val mod = new ModDefn(
      wiredInModName, 
      wiredInTyCons.map(exports(wiredInModName)))
    wiredInTyCons.foldLeft(mod)(_ addTyCon _)
  }

  def isWiredIn(con:GCon) = con match {
    case UnitCon | ArrowCon | ListCon | TupleCon(_) => true
    case Con(n) if n == nmBool => true
    case _ => false
  }
 
  /**
   * Primitive tycons: although not defined in an actual module,
   * they are exposed with `normal' syntax, meaning that they can
   * be referenced in the code in either qualified or unqualified
   * form.
   */

  /**
   * Some code to specify a primitive module.
   */
  abstract class PrimOp(name:Name) { 
    def toId(modName:Name, tyCon:TyCon):Id 
  }
  case class CafOp(name:Name) extends PrimOp(name) { 
    def toId(modName:Name, tycon:TyCon) = {
      val ty = conTy(tycon.name.qualify(modName))
      Id(name, VanillaId, PolyTy(tycon.tyvars, ty))
    }
  }
  case class ClosedOp(name:Name, arity:Int) extends PrimOp(name) {
     def toId(modName:Name, tycon:TyCon) = {
       val tc = conTy(tycon.name.qualify(modName))
       val ty = PolyTy(tycon.tyvars, Type.mkFun(List.fill(arity+1)(tc)))
       Id(name, VanillaId, ty)
     }
  }
  case class CmpOp(name:Name, arity:Int) extends PrimOp(name) {
    def toId(modName:Name, tycon:TyCon) = {
       val tc = conTy(tycon.name.qualify(modName))
       val bool = conTy(nmBool.qualify(wiredInModName))
       val ty = PolyTy(tycon.tyvars, Type.mkFun(List.fill(arity)(tc) :+ bool))
       Id(name, VanillaId, ty)      
    }
  }
  case class AdHocOp(name:Name, ty:Type) extends PrimOp(name) {
    def toId(modName:Name, tycon:TyCon) = Id(name, VanillaId, ty)    
  }
  class PrimModSpec(name:Name, tycon:TyCon, ops:List[PrimOp]) {
    def toModDefn = {
      val modDefn = new ModDefn(name, List(exports(name)(tycon)))
      ops.foldLeft(modDefn.addTyCon(tycon))(
        (m,op) => m.addId(op.toId(name,tycon)))
    }
  }

  /**
   * Primitive types available so far. 
   */
  private val nmInt = Name('Int)
  private val nmBigInt = Name('BigInt)
  private val nmDouble = Name('Double)
  private val nmChar = Name('Char)
  private val nmString = Name('String)
  
  private val primIntSpec = new PrimModSpec(Name(Symbol("bluejelly.Int")),
    primTyCon(nmInt), List(
      ClosedOp(Name('add), 2),
      ClosedOp(Name('sub), 2),
      ClosedOp(Name('mul), 2),
      ClosedOp(Name('div), 2),
      ClosedOp(Name('rem), 2),
      ClosedOp(Name('neg), 1),
      ClosedOp(Name('not), 1),
      ClosedOp(Name('and), 2),
      ClosedOp(Name('or),  2),
      ClosedOp(Name('xor), 2),
      ClosedOp(Name('shl), 2),
      ClosedOp(Name('shr), 2),
      ClosedOp(Name('lshr), 2),
      CmpOp(Name('eq), 2),
      CmpOp(Name('neq),2),
      CmpOp(Name('lt), 2),
      CmpOp(Name('gt), 2),
      CmpOp(Name('leq),2),
      CmpOp(Name('geq),2)      
    ))

  private val primDoubleSpec = new PrimModSpec(Name(Symbol("bluejelly.Double")),
    primTyCon(nmDouble), List(
      ClosedOp(Name('add), 2),
      ClosedOp(Name('sub), 2),
      ClosedOp(Name('mul), 2),
      ClosedOp(Name('div), 2),
      ClosedOp(Name('rem), 2),
      ClosedOp(Name('neg), 1),
      CmpOp(Name('eq), 2),
      CmpOp(Name('neq),2),
      CmpOp(Name('lt), 2),
      CmpOp(Name('gt), 2),
      CmpOp(Name('leq),2),
      CmpOp(Name('geq),2)
  ))

  // TODO: Add ops in the runtime and reflect them here
  private val primBigIntSpec = new PrimModSpec(Name(Symbol("bluejelly.BigInt")),
    primTyCon(nmBigInt), List(
      CafOp(Name('zero)),
      CafOp(Name('one)),
      AdHocOp(Name('fromInt), PolyTy(Nil, Type.mkFun(conTy(nmInt),conTy(nmBigInt)))),
      AdHocOp(Name('fromString), PolyTy(Nil, Type.mkFun(conTy(nmString),conTy(nmBigInt)))),
      ClosedOp(Name('add), 2),
      ClosedOp(Name('mul), 2)
  ))

  // TODO: Complete this module
  private val primCharSpec = new PrimModSpec(
    Name(Symbol("bluejelly.Char")), primTyCon(nmChar), Nil)

  // TODO: Complete this module
  private val primStringSpec = new PrimModSpec(
    Name(Symbol("bluejelly.String")), primTyCon(nmString), Nil)

  /**
   * Built-in modules for primitive types.
   */
  def primMods = List(
    primIntSpec, 
    primDoubleSpec, 
    primBigIntSpec, 
    primCharSpec, 
    primStringSpec).map(_.toModDefn)

  // ---------------------------------------------------------------------
  // Helper stuff
  // ---------------------------------------------------------------------

  private def exports(modName:Name)(tycon:TyCon) =  
    ExportedTc(
      tycon.name.qualify(modName), 
      tycon.dcons.map(_.name.qualify(modName)))

  private def primTyCon(name:Name) =
    TyCon(name, Nil, Nil, Nil)

  private def unitTyCon = 
    primTyCon(nmUnit)
 
  private def arrowTyCon = 
    TyCon(nmArrow, Nil, List(tyVar('a),tyVar('b)), Nil) 

  private def boolTyCon = {
    val boolType = PolyTy(Nil,conTy(nmBool.qualify(wiredInModName)))
    val `false` = DataCon(nmFalse, boolType, Nil, Nil)
    val `true` = DataCon(nmTrue, boolType, Nil, Nil)
    val bool = TyCon(nmBool, Nil, Nil, List(`false`,`true`))
    `false`.tycon = bool
    `true`.tycon = bool
    bool
  }

  private def listTyCon = {
    val nilType = PolyTy(List(tyVar('a)), AppTy(TcTy(ListCon),tvTy('a)))

    val consType = PolyTy(List(tyVar('a)), mkFun(List(
        tvTy('a), 
        AppTy(TcTy(ListCon),tvTy('a)), 
        AppTy(TcTy(ListCon),tvTy('a)))))

    val nil = DataCon(nmList, nilType, Nil, Nil)
    val cons = DataCon(nmCons, consType, List(false,false), Nil)
    val list = TyCon(nmList, Nil, List(tyVar('a)), List(nil,cons))
    nil.tycon = list
    cons.tycon = list
    list
  }
}
