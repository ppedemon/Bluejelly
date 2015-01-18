/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.core

import bluejelly.bjc.common.{TcRef,ConRef,ListRef,UnitRef,ArrowRef,TupleRef}
import bluejelly.bjc.common.{ExportInfo, ExportedId,ExportedTc,Fixity}
import bluejelly.bjc.common.{Name,ScopedName}

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
   * (except for type bool), in a special unnamed module. Wired-in 
   * tycons are:
   *  
   *   - arrow: data (->)
   *   - unit:  data ()
   *   - bool:  data Bool = False | True
   *   - lists: data [a] = [] | a:[a]
   *
   * Tuples (other than unit) are magic: since they are an infinite
   * family of tycons, they don't belong to the unnamed module. The 
   * environment will have to handle them as a special case, but
   * they are still considered to be wired-in tycons. 
   */

  // Wired-in module is unnamed, so you can't import it explicitly
  val wiredInModName = Symbol("bluejelly.WiredIn")

  private val nmArrow = ScopedName.tcName('->)
  private val nmUnit = ScopedName.tcName(Symbol("()"))
  private val nmList = ScopedName.tcName(Symbol("[]"))
  private val nmNil = ScopedName.idName(Symbol("[]"))
  private val nmCons = ScopedName.idName(':)
  private val nmBool = ScopedName.tcName('Bool)
  private val nmTrue = ScopedName.idName('True)
  private val nmFalse = ScopedName.idName('False)

  def wiredInMod = {
    val wiredInTyCons = List(unitTyCon, arrowTyCon, boolTyCon, listTyCon)
    val mod = new ModDefn(
      wiredInModName, 
      wiredInTyCons.map(exportTc(wiredInModName)))
    wiredInTyCons.foldLeft(mod)(_ addTyCon _)
  }

  def isWiredIn(tcRef:TcRef) = tcRef match {
    case UnitRef | ArrowRef | ListRef | TupleRef(_) => true
    case ConRef(n) if n.name == nmBool => true
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
  abstract class PrimOp(val name:ScopedName) { 
    def toId(modName:Symbol, tyCon:TyCon):Id 
  }
  case class CafOp(override val name:ScopedName) extends PrimOp(name) { 
    def toId(modName:Symbol, tycon:TyCon) = {
      val ty = conTy(Name.qualName(modName, tycon.name))
      Id(name, VanillaId, PolyTy(tycon.tyvars, ty))
    }
  }
  case class ClosedOp(override val name:ScopedName, arity:Int) extends PrimOp(name) {
     def toId(modName:Symbol, tycon:TyCon) = {
       val tc = conTy(Name.qualName(modName, tycon.name))
       val ty = PolyTy(tycon.tyvars, Type.mkFun(List.fill(arity+1)(tc)))
       Id(name, VanillaId, ty)
     }
  }
  case class CmpOp(override val name:ScopedName, arity:Int) extends PrimOp(name) {
    def toId(modName:Symbol, tycon:TyCon) = {
       val tc = conTy(Name.qualName(modName, tycon.name))
       val bool = conTy(Name.qualName(wiredInModName, nmBool))
       val ty = PolyTy(tycon.tyvars, Type.mkFun(List.fill(arity)(tc) :+ bool))
       Id(name, VanillaId, ty)
    }
  }
  case class AdHocOp(override val name:ScopedName, ty:Type) extends PrimOp(name) {
    def toId(modName:Symbol, tycon:TyCon) = Id(name, VanillaId, ty)    
  }
  class PrimModSpec(name:Symbol, tycon:TyCon, ops:List[PrimOp]) {
    def toModDefn = {
      val exps = exportTc(name)(tycon) :: (ops map exportId(name))
      val modDefn = new ModDefn(name, exps)
      ops.foldLeft(modDefn.addTyCon(tycon))(
        (m,op) => m.addId(op.toId(name,tycon)))
    }
  }

  /**
   * Primitive types available so far. 
   */
  private val nmInt = ScopedName.tcName('Int)
  private val nmBigInt = ScopedName.tcName('BigInt)
  private val nmDouble = ScopedName.tcName('Double)
  private val nmChar = ScopedName.tcName('Char)
  private val nmString = ScopedName.tcName('String)

  private val intMod = Symbol("bluejelly.Int")
  private val doubleMod = Symbol("bluejelly.Double")
  private val bigIntMod = Symbol("bluejelly.BigInt")
  private val charMod = Symbol("bluejelly.Char")
  private val stringMod = Symbol("bluejelly.String")

  private val primIntSpec = new PrimModSpec(intMod,
    primTyCon(nmInt), List(
      ClosedOp(ScopedName.idName('add), 2),
      ClosedOp(ScopedName.idName('sub), 2),
      ClosedOp(ScopedName.idName('mul), 2),
      ClosedOp(ScopedName.idName('div), 2),
      ClosedOp(ScopedName.idName('rem), 2),
      ClosedOp(ScopedName.idName('neg), 1),
      ClosedOp(ScopedName.idName('not), 1),
      ClosedOp(ScopedName.idName('and), 2),
      ClosedOp(ScopedName.idName('or),  2),
      ClosedOp(ScopedName.idName('xor), 2),
      ClosedOp(ScopedName.idName('shl), 2),
      ClosedOp(ScopedName.idName('shr), 2),
      ClosedOp(ScopedName.idName('lshr), 2),
      CmpOp(ScopedName.idName('eq), 2),
      CmpOp(ScopedName.idName('neq),2),
      CmpOp(ScopedName.idName('lt), 2),
      CmpOp(ScopedName.idName('gt), 2),
      CmpOp(ScopedName.idName('leq),2),
      CmpOp(ScopedName.idName('geq),2)      
    ))

  private val primDoubleSpec = new PrimModSpec(doubleMod,
    primTyCon(nmDouble), List(
      ClosedOp(ScopedName.idName('add), 2),
      ClosedOp(ScopedName.idName('sub), 2),
      ClosedOp(ScopedName.idName('mul), 2),
      ClosedOp(ScopedName.idName('div), 2),
      ClosedOp(ScopedName.idName('rem), 2),
      ClosedOp(ScopedName.idName('neg), 1),
      CmpOp(ScopedName.idName('eq), 2),
      CmpOp(ScopedName.idName('neq),2),
      CmpOp(ScopedName.idName('lt), 2),
      CmpOp(ScopedName.idName('gt), 2),
      CmpOp(ScopedName.idName('leq),2),
      CmpOp(ScopedName.idName('geq),2)
  ))

  private val primBigIntSpec = new PrimModSpec(bigIntMod,
    primTyCon(nmBigInt), List(
      CafOp(ScopedName.idName('zero)),
      CafOp(ScopedName.idName('one)),
      AdHocOp(ScopedName.idName('fromInt), PolyTy(Nil, Type.mkFun(
        conTy(Name.qualName(intMod, nmInt)),
        conTy(Name.qualName(bigIntMod, nmBigInt))))),
      AdHocOp(ScopedName.idName('fromString), PolyTy(Nil, Type.mkFun(
        conTy(Name.qualName(stringMod, nmString)),
        conTy(Name.qualName(bigIntMod, nmBigInt))))),
      ClosedOp(ScopedName.idName('add), 2),
      ClosedOp(ScopedName.idName('mul), 2)
  ))

  // TODO: Complete this module
  private val primCharSpec = new PrimModSpec(
    charMod, primTyCon(nmChar), Nil)

  // TODO: Complete this module
  private val primStringSpec = new PrimModSpec(
    stringMod, primTyCon(nmString), Nil)

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

  private def exportTc(modName:Symbol)(tycon:TyCon) = {
    val n = Name.qualName(modName, tycon.name)
    ExportedTc(n, n::tycon.dcons.map(d => Name.qualName(modName,d.name)))
  }

  private def exportId(modName:Symbol)(op:PrimOp) = 
    ExportedId(Name.qualName(modName, op.name))

  private def primTyCon(name:ScopedName) =
    TyCon(name, Nil, Nil, Nil)

  private def unitTyCon = 
    primTyCon(nmUnit)
 
  private def arrowTyCon = 
    TyCon(nmArrow, Nil, List(tyVar('a),tyVar('b)), Nil) 

  private def boolTyCon = {
    val boolType = PolyTy(Nil,conTy(Name.qualName(wiredInModName, nmBool)))
    val `false` = DataCon(nmFalse, boolType, Nil, Nil, 0)
    val `true` = DataCon(nmTrue, boolType, Nil, Nil, 1)
    val bool = TyCon(nmBool, Nil, Nil, List(`false`,`true`))
    `false`.tycon = bool
    `true`.tycon = bool
    bool
  }

  private def listTyCon = {
    val nilType = PolyTy(List(tyVar('a)), AppTy(TcTy(ListRef),tvTy('a)))

    val consType = PolyTy(List(tyVar('a)), mkFun(List(
        tvTy('a), 
        AppTy(TcTy(ListRef),tvTy('a)), 
        AppTy(TcTy(ListRef),tvTy('a)))))

    val nil = DataCon(nmNil, nilType, Nil, Nil, 0)
    val cons = DataCon(nmCons, consType, List(false,false), Nil, 1)
    val list = TyCon(nmList, Nil, List(tyVar('a)), List(nil,cons))
    nil.tycon = list
    cons.tycon = list
    list
  }
}
