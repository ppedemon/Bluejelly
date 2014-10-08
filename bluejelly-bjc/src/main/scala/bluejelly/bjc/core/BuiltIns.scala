/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.core

import bluejelly.bjc.ast.{GCon,UnitCon,TupleCon,ArrowCon,ListCon,Con}
import bluejelly.bjc.common.Name

import Type._

/**
 * Information about built-in stuff. Here we provide built-in
 * [[ModDefn]] instances for the constructs natively provided
 * by the language, such as lists or primitive types.
 *
 * Built in modules are special: it makes no sense to declare
 * private stuff in them, so everything exported. This makes
 * ModDefn.exports redundant, hence for built-in modules the
 * export list is empty and the import chaser assumes that
 * all declarations are visible. 
 *
 * @author ppedemon
 */
object BuiltIns {

  private val primsModName = Name(Symbol("bluejelly.Prim"))

  private val nmInt = Name('Int)
  private val nmBigInt = Name('BigInt)
  private val nmDouble = Name('Double)
  private val nmChar = Name('Char)
  private val nmString = Name('String)
  private val nmArrow = Name('->)
  private val nmUnit = Name(Symbol("()"))
  private val nmList = Name(Symbol("[]"))
  private val nmCons = Name(':)

  val builtInNames = Set(
    nmInt,
    nmBigInt,
    nmDouble,
    nmChar,
    nmString,
    nmUnit,
    nmArrow,
    nmList
  )

  val builtInTyCons = Map(
    nmInt -> primTyCon(nmInt),
    nmBigInt -> primTyCon(nmBigInt),
    nmDouble -> primTyCon(nmDouble),
    nmChar -> primTyCon(nmChar),
    nmString -> primTyCon(nmString),
    nmUnit -> primTyCon(nmUnit),
    nmArrow -> arrowTyCon,
    nmList -> listTyCon
  )

  def isBuiltIn(con:GCon) = con match {
    case UnitCon | ArrowCon | ListCon | TupleCon(_) => true
    case Con(n) if builtInNames(n) => true
    case _ => false
  }

  /**
   * Built-in module for primitive types: lists, arrow, unit,
   * and basic stuff like Int, BigInt, Double, Char, etc.
   */
  def primsMod = {
    val mod = new ModDefn(primsModName)
    builtInTyCons.values.foldLeft(mod)(_ addTyCon _)
  }

  // ---------------------------------------------------------------------
  // Private definitions
  // ---------------------------------------------------------------------

  private def primTyCon(name:Name) =
    TyCon(name, false, Nil, Nil, Nil)

  private def arrowTyCon = 
    TyCon(nmArrow, false, Nil, List(tyVar('a),tyVar('b)), Nil) 

  private def listTyCon = {
    val nilType = PolyTy(List(tyVar('a)), AppTy(TcTy(ListCon),tvTy('a)))

    val consType = PolyTy(List(tyVar('a)), mkFun(List(
        tvTy('a), 
        AppTy(TcTy(ListCon),tvTy('a)), 
        AppTy(TcTy(ListCon),tvTy('a)))))

    val nil = DataCon(nmList, nilType, Nil, Nil)
    val cons = DataCon(nmCons, consType, List(false,false), Nil)
    val list = TyCon(nmList, false, Nil, List(tyVar('a)), List(nil,cons))
    nil.tycon = list
    cons.tycon = list
    list
  }
}
